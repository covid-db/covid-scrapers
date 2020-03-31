{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Covid19.USA.Utah where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Text.Lens
import           Data.Time
import           Network.Http.Client hiding (Connection, withConnection)
import           Numeric.Lens
import           OpenSSL
------------------------------------------------------------------------------
import           Covid19.Instances()
------------------------------------------------------------------------------

data UtahJurisdiction
  = BearRiver
  | CentralUtah
  | DavisCounty
  | SaltLakeCounty
  | SanJuan
  | SoutheastUtah
  | SouthwestUtah
  | SummitCounty
  | TooeleCounty
  | TriCounty
  | UtahCounty
  | WasatchCounty
  | WeberMorgan
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

jurisdictionText :: UtahJurisdiction -> Text
jurisdictionText = \case
  BearRiver -> "Bear River"
  CentralUtah -> "Central Utah"
  DavisCounty -> "Davis County"
  SaltLakeCounty -> "Salt Lake County"
  SanJuan -> "San Juan"
  SoutheastUtah -> "Southeast Utah"
  SouthwestUtah -> "Southwest Utah"
  SummitCounty -> "Summit County"
  TooeleCounty -> "Tooele County"
  TriCounty -> "TriCounty"
  UtahCounty -> "Utah County"
  WasatchCounty -> "Wasatch County"
  WeberMorgan -> "Weber-Morgan"

data UtahReport = UtahReport
  { utahReport_date :: Day
  , utahReport_jurisdiction :: Text
  , utahReport_utahCases :: Int
  , utahReport_visitorCases :: Int
  } deriving (Eq,Ord,Show,Read)

instance C.ToNamedRecord UtahReport where
    toNamedRecord (UtahReport d j u v) = C.namedRecord
      [ "date" C..= d
      , "jurisdiction" C..= j
      , "utah_cases" C..= u
      , "visitor_cases" C..= v
      ]
instance C.DefaultOrdered UtahReport where
    headerOrder _ = C.header ["date", "jurisdiction", "utah_cases", "visitor_cases"]

scrapeUtah :: IO ()
scrapeUtah = do
  withOpenSSL $ do
    md <- getUtahData
    case md of
      Left e -> putStrLn e
      Right d -> do
        putStrLn "Parsed Utah case data successfully."
        let cfg = C.defaultEncodeOptions { C.encUseCrLf = False }
        BL.appendFile "utah.csv" $ C.encodeDefaultOrderedByNameWith cfg d

getUtahData :: IO (Either String [UtahReport])
getUtahData = do
    (UTCTime d _) <- getCurrentTime
    page <- get "https://coronavirus-dashboard.utah.gov/" concatHandler
    let t = fst $ T.breakOn "</script>" $ T.drop 1 $
            snd $ T.breakOn ">" $
            snd $ T.breakOn "<script" $
            snd $ T.breakOn "number-of-lab-confirmed-covid-19-cases-in-utah" $
            decodeUtf8 page
    case t ^? key "x" . key "data" of
      Nothing -> return $ Left "Error: Data format changed"
      Just a -> do
        let counties = a ^.. nth 0 . values . _String
            utahCases = a ^.. nth 1 . values . _String . unpacked . decimal
            visitorCases = a ^.. nth 2 . values . _String . unpacked . decimal
        return $ Right $ zipWith3 (UtahReport d) counties utahCases visitorCases
