{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Data.Oanda.DecimalNumber
  ( DecimalNumber(..)
  , Quantity
  , prettyDecimalNumber
  , setDecimalPrecision
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Text        (pack, unpack)
import           GHC.Generics
import           Text.PrettyPrint
import           Text.Printf

type Precision = Int
type Quantity = DecimalNumber

data DecimalNumber =
  DecimalNumber (Maybe Precision) Float
  deriving (Show, Eq, Ord, Generic, NFData, Num, Fractional)

instance FromJSON DecimalNumber where
  parseJSON (String v) = return $ DecimalNumber Nothing (read $ unpack v)
  parseJSON v = fail $ "Cannot parse non string to nubmer (value was '" ++ show v ++ "') in parseJSON of DecimalNumber"

instance ToJSON DecimalNumber where
  toJSON (DecimalNumber (Just prec) x) = String $ pack (printf ("%." ++ show prec ++ "f") x :: String)
  toJSON (DecimalNumber Nothing x) = String $ pack (printf "%.4f" x :: String)


setDecimalPrecision :: Int -> DecimalNumber -> DecimalNumber
setDecimalPrecision prec (DecimalNumber _ f) = DecimalNumber (Just prec) f

prettyDecimalNumber :: DecimalNumber -> Doc
prettyDecimalNumber (DecimalNumber (Just prec) f) = text $ printf ("%." ++ show prec ++ "f") f
prettyDecimalNumber (DecimalNumber Nothing f) = text $ printf "%.4f" f
