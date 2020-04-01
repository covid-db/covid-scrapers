{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Covid19.USA.NewJersey where

------------------------------------------------------------------------------
import           Control.Lens
import           Covid19.Instances        ()
import qualified Control.Error as ER
import           Data.Aeson               (Value (..))
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Csv                 as C
import           Data.Scientific          (toBoundedInteger)
import           Data.Text                (Text)
import           Data.Time
import qualified Data.Vector              as V
import           Network.Http.Client      hiding (Connection, withConnection)
import           OpenSSL

jsonUrl :: URL
jsonUrl = "https://services7.arcgis.com/Z0rixLlManVefxqY/arcgis/rest/services/DailyCaseCounts/FeatureServer/0/query?f=json&where=1=1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=TOTAL_CASE desc&resultOffset=0&resultRecordCount=25&cacheHint=true"

data NewJerseyReport = NewJerseyReport
  { newJerseyReport_date   :: Day
  , newJerseyReport_county :: Text
  , newJerseyReport_cases  :: Int
  , newJerseyReport_deaths :: Int
  } deriving (Eq, Ord, Show, Read)

instance C.ToNamedRecord NewJerseyReport where
  toNamedRecord (NewJerseyReport d c cs ds) = C.namedRecord
    [ "date" C..= d
    , "county" C..= c
    , "cases" C..= cs
    , "deaths" C..= ds
    ]

instance C.DefaultOrdered NewJerseyReport where
  headerOrder _ = C.header ["date", "county", "cases", "deaths"]

scrapeNewJersey :: IO ()
scrapeNewJersey =
  withOpenSSL $ do
    md <- getNewJerseyData
    case md of
      Left e -> putStrLn e
      Right d -> do
        putStrLn "Parsed New Jersey case data successfully."
        let cfg = C.defaultEncodeOptions { C.encUseCrLf = False }
        BL.appendFile "newjersey.csv" $ C.encodeDefaultOrderedByNameWith cfg d

getNewJerseyData :: IO (Either String [NewJerseyReport])
getNewJerseyData = do
  (UTCTime d _) <- getCurrentTime
  jsonBs <- get jsonUrl concatHandler
  let mArrayOfData = jsonBs ^? key "features"
  case mArrayOfData of
    Nothing -> return $ Left "Could not find expected 'features' array in json payload"
    Just dataArray ->
      case dataArray of
        Array vec -> do
          mNJList <-
             V.mapM
                (\item -> ER.runMaybeT $ do
                  county <- ER.hoistMaybe $ item ^? key "attributes" . key "COUNTY_LAB" . _String
                  cases <- ER.hoistMaybe $ item ^? key "attributes" . key "TOTAL_CASE" . _Number
                  deaths <- ER.hoistMaybe $ item ^? key "attributes" . key "DEATHS" . _Number
                  casesInt <- ER.hoistMaybe (toBoundedInteger cases)
                  deathsInt <- ER.hoistMaybe (toBoundedInteger deaths)
                  return $
                    NewJerseyReport
                      { newJerseyReport_date   = d
                      , newJerseyReport_county = county
                      , newJerseyReport_cases  = casesInt
                      , newJerseyReport_deaths = deathsInt
                      }
                ) vec
          return $ Right (ER.catMaybes $ V.toList mNJList)
        _ -> return $ Left "Received data is not an array"
