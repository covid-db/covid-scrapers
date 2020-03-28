{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Covid19 where

------------------------------------------------------------------------------
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.Csv as C
import           Data.Csv.Lens
import           Data.List (sort)
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text.Read as T
import           Data.Time
import           Network.Http.Client hiding (Connection, withConnection)
import           OpenSSL
------------------------------------------------------------------------------

data Env = Env
  { _env_now :: UTCTime
  --, _env_dbConnectInfo :: DbConnect
  }

data Column = StateCol | CountryCol | LatCol | LonCol
  deriving (Eq,Ord,Show,Read,Enum)

colName :: Column -> Text
colName StateCol = "Province/State"
colName CountryCol = "Country/Region"
colName LatCol = "Lat"
colName LonCol = "Long"

parseCol :: MonadPlus m => Text -> m Column
parseCol "Province/State" = pure StateCol
parseCol "Country/Region" = pure CountryCol
parseCol "Lat" = pure LatCol
parseCol "Long" = pure LonCol
parseCol _ = mzero

instance C.ToField Column where
  toField = toS . colName

instance C.FromField Column where
  parseField = parseCol . toS

instance C.ToField Day where
  toField = toS . formatTime defaultTimeLocale "%D"

instance C.FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%D" . toS

runScraper :: Env -> IO ()
runScraper env = do
  withOpenSSL $ do
    let prefix = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
    confirmed <- get (prefix <> "time_series_covid19_confirmed_global.csv") concatHandler
    deaths <- get (prefix <> "time_series_covid19_deaths_global.csv") concatHandler
    recovered <- get (prefix <> "time_series_covid19_recovered_global.csv") concatHandler
    let emap = do
          c <- sequence $ recordToMap (\n -> mempty { pConfirmed = n }) <$> (toS confirmed ^.. namedCsv . rows)
          d <- sequence $ recordToMap (\n -> mempty { pDeaths = n }) <$> (toS deaths ^.. namedCsv . rows)
          r <- sequence $ recordToMap (\n -> mempty { pRecovered = n }) <$> (toS recovered ^.. namedCsv . rows)
          pure $ M.unionsWith mappend (c <> d <> r)
    case emap of
      Left e -> do
        putStrLn "Unrecoverable rror in scraped data:"
        putStrLn e
      Right m -> do
        pidMap <- insertPlaces env $ S.toList $ S.fromList $ map fst $ M.keys m
        insertMap env pidMap $ M.toList m

data SimplePlace = SimplePlace
  { spCountry :: Text
  , spRegion :: Maybe Text
  , spLat :: Double
  , spLon :: Double
  } deriving (Eq,Ord,Show)

data Payload = Payload
  { pConfirmed :: Int
  , pDeaths :: Int
  , pRecovered :: Int
  } deriving (Eq,Ord,Show)

instance Semigroup Payload where
  Payload ac ad ar <> Payload bc bd br = Payload (ac+bc) (ad+bd) (ar+br)

instance Monoid Payload where
  mempty = Payload 0 0 0
  mappend (Payload ac ad ar) (Payload bc bd br) = Payload (ac+bc) (ad+bd) (ar+br)

recordToMap
  :: (Int -> Payload)
  -> CsvRecord C.Name
  -> Either String (Map (SimplePlace, Day) Payload)
recordToMap mkPayload record = do
    let c = record ^. ix (toS $ colName CountryCol)
        s = record ^. ix (toS $ colName StateCol)
    (lat,_) <- T.double $ toS $ record ^. ix (toS $ colName LatCol)
    (lon,_) <- T.double $ toS $ record ^. ix (toS $ colName LonCol)
    let sp = SimplePlace (toS c) (Just $ toS s) lat lon
    m <- note "Error parsing NamedRecord" $ record ^? _NamedRecord @(Map String Text)
    fmap M.unions $ forM (M.toList m) $ \(fn, val) -> do
      case parseDay fn of
        Nothing -> pure mempty
        Just d -> do
          (v,_) <- T.decimal val
          pure (M.singleton (sp, d) $ mkPayload v)

type PlaceId = Int
insertPlaces :: Env -> [SimplePlace] -> IO (Map SimplePlace PlaceId)
insertPlaces env places = return mempty
--insertPlaces env places = withConnection (_env_dbConnectInfo env) $ \conn ->
--    runBeamPostgres conn $ fmap M.unions $ forM places $ \sp@(SimplePlace c r lat lon) -> do
--      ps <- runInsertReturningList $ insert (_covidDb_places covidDb) $
--          insertExpressions [Place default_ (val_ $ toS c) (val_ r) (val_ lat) (val_ lon)]
--      case ps of
--        [p] -> return $ M.singleton sp $ pk p
--        _ -> pure mempty
--
insertMap :: Env -> Map SimplePlace PlaceId -> [((SimplePlace, Day), Payload)] -> IO ()
insertMap env pidMap rows = return ()
--insertMap env pidMap rows = withConnection (_env_dbConnectInfo env) $ \conn ->
--    runBeamPostgres conn $ forM_ rows $ \((sp, d), (Payload p1 p2 p3)) -> do
--      runInsert $ insert (_covidDb_reports covidDb) $ insertValues
--        [Report (_env_now env) (pidMap M.! sp) d p1 p2 p3]

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%-m/%-d/%y"

