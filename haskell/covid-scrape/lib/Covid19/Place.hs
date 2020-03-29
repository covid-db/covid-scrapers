{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Covid19.Place where

------------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.Csv as C
import           Data.Text (Text)
import           Database.Beam
import           Database.Beam.Backend.SQL.Types
------------------------------------------------------------------------------

------------------------------------------------------------------------------
data PlaceT f = Place
  { place_id :: C f (SqlSerial Int)
  , place_country :: C f Text
  , place_region :: C f (Maybe Text)
  , place_lat :: C f Double
  , place_lon :: C f Double
  } deriving (Generic)

type Place = PlaceT Identity
type PlaceId = PrimaryKey PlaceT Identity

deriving instance Eq (PrimaryKey PlaceT Identity)
deriving instance Eq Place
deriving instance Ord (PrimaryKey PlaceT Identity)
deriving instance Ord Place
deriving instance Show (PrimaryKey PlaceT Identity)
deriving instance Show Place

instance ToJSON (PrimaryKey PlaceT Identity) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (PrimaryKey PlaceT Identity)

instance ToJSON Place where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Place

instance C.ToNamedRecord Place where
    toNamedRecord (Place (SqlSerial i) c r lat lon) = C.namedRecord
      [ "id" C..= i
      , "country" C..= c
      , "region" C..= r
      , "lat" C..= lat
      , "lon" C..= lon
      ]
instance C.DefaultOrdered Place where
    headerOrder _ = C.header ["id", "country", "region", "lat", "lon"]

instance Beamable PlaceT

instance Table PlaceT where
  data PrimaryKey PlaceT f = PlaceId (Columnar f (SqlSerial Int))
    deriving (Generic, Beamable)
  primaryKey = PlaceId . place_id
