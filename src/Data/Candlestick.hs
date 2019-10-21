

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Candlestick
    ( Candlestick
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics

import           Data.CandlestickData
import           Data.DateTime

data Candlestick = Candlestick
  { time     :: DateTime              -- ^ The start time of the candlestick
  , bid      :: Maybe CandlestickData -- ^ The candlestick data based on bids. Only provided if bid-based candles were requested.
  , ask      :: Maybe CandlestickData -- ^ The candlestick data based on asks. Only provided if ask-based candles were requested.
  , mid      :: Maybe CandlestickData -- ^ The candlestick data based on midpoints. Only provided if midpoint-based candles were requested.
  , volume   :: Int                   -- ^ The number of prices created during the time-range represented by the candlestick.
  , complete :: Bool                  -- ^ A flag indicating if the candlestick is complete. A complete candlestick is one whose ending time is not in the future.
  } deriving (Show, Eq, Ord, Generic, FromJSON, NFData)
