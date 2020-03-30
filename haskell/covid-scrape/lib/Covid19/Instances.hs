module Covid19.Instances where

import Data.Csv
import Data.String.Conv
import Data.Time

instance ToField Day where
  toField = toS . formatTime defaultTimeLocale "%D"

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%D" . toS
