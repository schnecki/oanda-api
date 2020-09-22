{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Data.Oanda.NaturalNumber
  ( NaturalNumber(..)
  , Quantity
  , prettyNaturalNumber
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Text        (pack, unpack)
import           Text.PrettyPrint
import           Text.Printf


type Quantity = NaturalNumber

newtype NaturalNumber =
  NaturalNumber Float
  deriving (Show, Eq, Ord, NFData, Num, Fractional)

instance FromJSON NaturalNumber where
  parseJSON (String v) = return $ NaturalNumber (read $ unpack v)
  parseJSON v = fail $ "Cannot parse non string to nubmer (value was '" ++ show v ++ "') in parseJSON of NaturalNumber"

instance ToJSON NaturalNumber where
  toJSON (NaturalNumber x) = String $ pack (printf "%d" x :: String)


prettyNaturalNumber :: NaturalNumber -> Doc
prettyNaturalNumber (NaturalNumber x) = text $ printf "%d" x
