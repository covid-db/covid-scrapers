{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Covid19.USA.Alabama where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Aeson.Lens
import qualified Data.ByteString as B
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
import           Text.HTML.TagSoup.Parsec
import           Text.Parsec
------------------------------------------------------------------------------
import           Covid19.Instances()
------------------------------------------------------------------------------

data AlabamaReport = AlabamaReport
  { alabamaReport_date :: Day
  , alabamaReport_county :: Text
  , alabamaReport_cases :: Int
  , alabamaReport_deaths :: Int
  } deriving (Eq,Ord,Show,Read)

instance C.ToNamedRecord AlabamaReport where
    toNamedRecord (AlabamaReport d c cs ds) = C.namedRecord
      [ "date" C..= d
      , "county" C..= c
      , "cases" C..= cs
      , "deaths" C..= ds
      ]
instance C.DefaultOrdered AlabamaReport where
    headerOrder _ = C.header ["date", "county", "cases", "deaths"]

scrapeAlabama :: IO ()
scrapeAlabama = do
  withOpenSSL $ do
    md <- getAlabamaData
    case md of
      Left e -> putStrLn e
      Right d -> do
        putStrLn "Parsed Alabama case data successfully."
        let cfg = C.defaultEncodeOptions { C.encUseCrLf = False }
        BL.appendFile "Alabama.csv" $ C.encodeDefaultOrderedByNameWith cfg d

getAlabamaData :: IO (Either String [AlabamaReport])
getAlabamaData = do
    (UTCTime d _) <- getCurrentTime
    page <- get "https://www.alabamapublichealth.gov/infectiousdiseases/2019-coronavirus.html" concatHandler
    let tags = filter (not . isWhitespaceTag) $ parseTags $ decodeUtf8 page
    mapM_ print tags
    let a = tParse alabamaCases tags
    return $ Right $ map (\(d,c,cs,ds) -> AlabamaReport d c (read $ T.unpack cs) (read $ T.unpack ds)) a

isWhitespaceTag (TagText t) = T.null $ T.strip t
isWhitespaceTag _ = False

alabamaCases :: TagParser Text [(Text, Text)]
alabamaCases = do
  many notDeaths
  textTag "Deaths"
  closeTag "th"
  closeTag "tr"
  closeTag "thead"
  cs <- many (try countyCases)
  manyTill anyToken eof
  return cs

notDeaths = tagEater f
  where
    f tag@(TagText t) = if t == "Deaths" then Nothing else Just tag
    f t = Just t

countyCases = do
  openTag "tr"
  openTag "td"
  county <- textTagContents
  closeTag "td"
  openTag "td"
  county <- textTagContents
  closeTag "td"
  openTag "td"
  deaths <- textTagContents
  closeTag "td"
  closeTag "tr"
  return (county, cases, deaths)

textTagContents :: Show t => ParsecT [Tag t] () Identity t
textTagContents = tagEater f
  where
    f (TagText t) = Just t
    f _ = Nothing
