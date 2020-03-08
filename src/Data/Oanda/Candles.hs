{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.Candles
    ( Candles
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics

import           Data.Oanda.Candlestick
import           Data.Oanda.CandlestickGranularity
import           Data.Oanda.Types

data Candles = Candles
  { instrument  :: InstrumentName         -- ^ The instrument whose Prices are represented by the candlesticks.
  , granularity :: CandlestickGranularity -- ^ The granularity of the candlesticks provided.
  , candles     :: [Candlestick]          -- ^ The list of candlesticks that satisfy the request.
  } deriving (Show, Eq, Ord, Generic, FromJSON, NFData)
