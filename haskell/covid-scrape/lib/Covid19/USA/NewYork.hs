{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Covid19.USA.NewYork where

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

data NYReport = NYReport
  { nyReport_date :: Day
  , nyReport_county :: Text
  , nyReport_positiveCases :: Int
  } deriving (Eq,Ord,Show,Read)

instance C.ToNamedRecord NYReport where
    toNamedRecord (NYReport d c pc) = C.namedRecord
      [ "date" C..= d
      , "county" C..= c
      , "cases" C..= pc
      ]
instance C.DefaultOrdered NYReport where
    headerOrder _ = C.header ["date", "county", "cases"]

scrapeNewYork :: IO ()
scrapeNewYork = do
  withOpenSSL $ do
    md <- getNewYorkData
    case md of
      Left e -> putStrLn e
      Right d -> do
        putStrLn "Parsed NY case data successfully."
        let cfg = C.defaultEncodeOptions { C.encUseCrLf = False }
        BL.appendFile "newyork.csv" $ C.encodeDefaultOrderedByNameWith cfg d

getNewYorkData :: IO (Either String [NYReport])
getNewYorkData = do
    (UTCTime d _) <- getCurrentTime
    page <- get "https://coronavirus.health.ny.gov/county-county-breakdown-positive-cases" concatHandler
    let tags = filter (not . isWhitespaceTag) $ parseTags $ decodeUtf8 page
    mapM_ print tags
    let a = tParse nyCases tags
    return $ Right $ map (\(c,n) -> NYReport d c (read $ T.unpack $ T.filter (/=',') n)) a

isWhitespaceTag (TagText t) = T.null $ T.strip t
isWhitespaceTag _ = False

nyCases :: TagParser Text [(Text, Text)]
nyCases = do
  many notPositiveCases
  textTag "Positive Cases"
  closeTag "th"
  closeTag "tr"
  cs <- many (try countyCases)
  manyTill anyToken eof
  return cs

notPositiveCases = tagEater f
  where
    f tag@(TagText t) = if t == "Positive Cases" then Nothing else Just tag
    f t = Just t

countyCases = do
  openTag "tr"
  openTag "td"
  county <- textTagContents
  closeTag "td"
  openTag "td"
  cases <- textTagContents
  closeTag "td"
  closeTag "tr"
  return (county, cases)

textTagContents :: Show t => ParsecT [Tag t] () Identity t
textTagContents = tagEater f
  where
    f (TagText t) = Just t
    f _ = Nothing
