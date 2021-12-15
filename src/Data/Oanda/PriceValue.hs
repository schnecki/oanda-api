{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Data.Oanda.PriceValue
  ( PriceValue(..)
  , WorstPriceValue
  , prettyPriceValue
  , showPriceValue
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Scientific  (toRealFloat)
import           Data.Serialize
import           Data.Text        (pack, unpack)
import           GHC.Generics
import           Text.PrettyPrint
import           Text.Printf


type WorstPriceValue = PriceValue

-- ^ Price Value is encoded as a string by OANDA. The amount of precision provided depends on the Price’s Instrument.
newtype PriceValue =
  PriceValue
    { fromPriceValue :: Float
    }
  deriving (Show, Read, Eq, Ord, Serialize, Generic, NFData, Num, Fractional)

instance FromJSON PriceValue where
  parseJSON (String v) = return $ PriceValue (read $ unpack v)
  parseJSON (Number v) = return $ PriceValue (toRealFloat v)
  parseJSON v = fail $ "Cannot parse non string to nubmer (value was '" ++ show v ++ "') in parseJSON of PriceValue"

instance ToJSON PriceValue where
  toJSON (PriceValue x) = String $ pack (printf "%.4f" x :: String)

prettyPriceValue :: PriceValue -> Doc
prettyPriceValue = text . showPriceValue

showPriceValue :: PriceValue -> String
showPriceValue (PriceValue f) = printf "%.4f" f


