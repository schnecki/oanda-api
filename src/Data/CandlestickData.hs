

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.CandlestickData
    ( CandlestickData
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics

import           Data.PriceValue
import           Types

data CandlestickData = CandlestickData
  { o :: PriceValue             -- ^ The first (open) price in the time-range represented by the candlestick.
  , h :: PriceValue             -- ^ The highest price in the time-range represented by the candlestick.
  , l :: PriceValue             -- ^ The lowest price in the time-range represented by the candlestick.
  , c :: PriceValue             -- ^ The last (closing) price in the time-range represented by the candlestick.
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, NFData)
