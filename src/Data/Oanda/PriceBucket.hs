{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.PriceBucket
    ( PriceBucket (..)
    , prettyPriceBucket
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.NaturalNumber
import           Data.Oanda.PriceValue
import           Data.Oanda.Types

-- | A PriceBucket represents a price available for an amount of liquidity.
data PriceBucket = PriceBucket
  { price     :: PriceValue    -- ^ The Price offered by the PriceBucket
  , liquidity :: NaturalNumber -- ^ The amount of liquidity offered by the PriceBucket
  } deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, NFData)


prettyPriceBucket :: PriceBucket -> Doc
prettyPriceBucket bucket =
  colName "price" $$ nest nestCols (prettyPriceValue $ price bucket) $+$
  colName "liquidity" $$ nest nestCols (prettyNaturalNumber $ liquidity bucket)
