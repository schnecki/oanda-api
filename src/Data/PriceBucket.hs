{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.PriceBucket
    ( PriceBucket (..)

    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics

import           Data.PriceValue

data PriceBucket = PriceBucket
  { price     :: PriceValue -- ^ The Price offered by the PriceBucket
  , liquidity :: Int -- ^ The amount of liquidity offered by the PriceBucket
  } deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, NFData)
