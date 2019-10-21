{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.DateTime
    ( DateTime (..)

    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Time
import           Data.Time.RFC3339
import           GHC.Generics

newtype DateTime =
  DateTime (Maybe UTCTime)
  deriving (Generic, Eq, Ord, NFData)

instance Show DateTime where
  show (DateTime (Just time)) = formatTimeRFC3339 (utcToZonedTime utc time)
  show (DateTime Nothing)     = "0"

instance FromJSON DateTime where
  parseJSON (String v) = return $ DateTime $ zonedTimeToUTC <$> parseTimeRFC3339 v
  parseJSON v = fail $ "Cannot parse non string to UTCTime (value was '" ++ show v ++ "') in parseJSON of DateTime"

instance ToJSON DateTime where
  toJSON (DateTime Nothing)     = String "0"
  toJSON (DateTime (Just time)) = String $ formatTimeRFC3339 (utcToZonedTime utc time)
