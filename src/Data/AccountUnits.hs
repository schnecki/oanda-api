{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Data.AccountUnits
  ( AccountUnits(..)
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Text       (pack, unpack)
import           GHC.Generics
import           Text.Printf

newtype AccountUnits =
  AccountUnits Float
  deriving (Generic, Show, Eq, Ord, NFData)

instance FromJSON AccountUnits where
  parseJSON (String v) = return $ AccountUnits (read $ unpack v)
  parseJSON v = fail $ "Cannot parse non string to nubmer (value was '" ++ show v ++ "') in parseJSON of AccountUnits"

instance ToJSON AccountUnits where
  toJSON (AccountUnits v) = String (pack $ printf "%.4f" v)

