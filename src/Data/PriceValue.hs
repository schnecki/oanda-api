{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Data.PriceValue
  ( PriceValue(..)
  , WorstPriceValue
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Text       (pack, unpack)
import           Text.Printf


type WorstPriceValue = PriceValue

-- ^ Price Value. The amount of precision provided depends on the Price’s Instrument.
newtype PriceValue =
  PriceValue Float
  deriving (Show, Eq, Ord, NFData, Num, Fractional)

instance FromJSON PriceValue where
  parseJSON (String v) = return $ PriceValue (read $ unpack v)
  parseJSON v = fail $ "Cannot parse non string to nubmer (value was '" ++ show v ++ "') in parseJSON of PriceValue"

instance ToJSON PriceValue where
  toJSON (PriceValue x) = String $ pack (printf "%.4f" x :: String)

