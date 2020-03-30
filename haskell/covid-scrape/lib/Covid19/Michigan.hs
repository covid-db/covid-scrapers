{-# LANGUAGE OverloadedStrings #-}

module Covid19.Michigan where

------------------------------------------------------------------------------
import           Control.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Time
import           Network.Http.Client hiding (Connection, withConnection)
import           OpenSSL
import           Text.HTML.TagSoup.Parsec
import           Text.Parsec
------------------------------------------------------------------------------
import           Covid19.Instances()
------------------------------------------------------------------------------

data MichReport = MichReport
  { michReport_date :: Day
  , michReport_county :: Text
  , michReport_cases :: Int
  , michReport_deaths :: Int
  } deriving (Eq,Ord,Show,Read)

instance C.ToNamedRecord MichReport where
    toNamedRecord (MichReport d c cs ds) = C.namedRecord
      [ "date" C..= d
      , "county" C..= c
      , "cases" C..= cs
      , "deaths" C..= ds
      ]
instance C.DefaultOrdered MichReport where
    headerOrder _ = C.header ["date", "county", "cases", "deaths"]

scrapeMichigan :: IO ()
scrapeMichigan = do
  withOpenSSL $ do
    md <- getMichiganData
    case md of
      Left e -> putStrLn e
      Right d -> do
        putStrLn "Parsed Michigan case data successfully."
        let cfg = C.defaultEncodeOptions { C.encUseCrLf = False }
        BL.appendFile "michigan.csv" $ C.encodeDefaultOrderedByNameWith cfg d

getMichiganData :: IO (Either String [MichReport])
getMichiganData = do
    (UTCTime d _) <- getCurrentTime
    page <- get "https://www.michigan.gov/coronavirus/0,9753,7-406-98163_98173---,00.html" concatHandler
    let tags = filter (not . isWhitespaceTag) $ parseTags $ decodeUtf8 page
    mapM_ print tags
    let a = tParse michCases tags
    return $ Right $ map (\(c,cs,ds) -> MichReport d (T.strip c) (read $ T.unpack cs) (read $ T.unpack $ fromMaybe "0" ds)) a

isWhitespaceTag (TagText t) = T.null $ T.strip t
isWhitespaceTag _ = False

michCases :: TagParser Text [(Text, Text, Maybe Text)]
michCases = do
  many notDeaths
  textTag "Deaths"
  closeTag "strong"
  closeTag "td"
  closeTag "tr"
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
  cases <- textTagContents
  closeTag "td"
  openTag "td"
  deaths <- optionMaybe textTagContents
  closeTag "td"
  closeTag "tr"
  return (county, cases, deaths)

textTagContents :: Show t => ParsecT [Tag t] () Identity t
textTagContents = tagEater f
  where
    f (TagText t) = Just t
    f _ = Nothing
