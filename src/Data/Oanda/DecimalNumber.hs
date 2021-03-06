{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Data.Oanda.DecimalNumber
  ( DecimalNumber(..)
  , Quantity
  , prettyDecimalNumber
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Text        (pack, unpack)
import           Text.PrettyPrint
import           Text.Printf


type Quantity = DecimalNumber

newtype DecimalNumber =
  DecimalNumber Float
  deriving (Show, Eq, Ord, NFData, Num, Fractional)

instance FromJSON DecimalNumber where
  parseJSON (String v) = return $ DecimalNumber (read $ unpack v)
  parseJSON v = fail $ "Cannot parse non string to nubmer (value was '" ++ show v ++ "') in parseJSON of DecimalNumber"

instance ToJSON DecimalNumber where
  toJSON (DecimalNumber x) = String $ pack (printf "%.4f" x :: String)


prettyDecimalNumber :: DecimalNumber -> Doc
prettyDecimalNumber (DecimalNumber f) = text $ printf "%.4f" f
