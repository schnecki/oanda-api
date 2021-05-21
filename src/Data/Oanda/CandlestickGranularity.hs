{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.CandlestickGranularity
    ( CandlestickGranularity (..)
    , candlestickGranularityInSeconds
    , candlestickGranularityToNomialDiffTime
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           Data.Time.Clock
import           GHC.Generics    hiding (M1)


data CandlestickGranularity
  = S5  -- ^ 5 second candlesticks, minute alignment
  | S10 -- ^ 10 second candlesticks, minute alignment
  | S15 -- ^ 15 second candlesticks, minute alignment
  | S30 -- ^ 30 second candlesticks, minute alignment
  | M1  -- ^ 1 minute candlesticks, minute alignment
  | M2  -- ^ 2 minute candlesticks, hour alignment
  | M4  -- ^ 4 minute candlesticks, hour alignment
  | M5  -- ^ 5 minute candlesticks, hour alignment
  | M10 -- ^ 10 minute candlesticks, hour alignment
  | M15 -- ^ 15 minute candlesticks, hour alignment
  | M30 -- ^ 30 minute candlesticks, hour alignment
  | H1  -- ^ 1 hour candlesticks, hour alignment
  | H2  -- ^ 2 hour candlesticks, day alignment
  | H3  -- ^ 3 hour candlesticks, day alignment
  | H4  -- ^ 4 hour candlesticks, day alignment
  | H6  -- ^ 6 hour candlesticks, day alignment
  | H8  -- ^ 8 hour candlesticks, day alignment
  | H12 -- ^ 12 hour candlesticks, day alignment
  | D   -- ^ 1 day candlesticks, day alignment
  | W   -- ^ 1 week candlesticks, aligned to start of week
  | M   -- ^ 1 month candlesticks, aligned to first day of the month
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Serialize, Generic, ToJSON, FromJSON, NFData)


candlestickGranularityInSeconds :: CandlestickGranularity -> Double
candlestickGranularityInSeconds S5  = 5
candlestickGranularityInSeconds S10 = 10
candlestickGranularityInSeconds S15 = 15
candlestickGranularityInSeconds S30 = 30
candlestickGranularityInSeconds M1  = 60
candlestickGranularityInSeconds M2  = 120
candlestickGranularityInSeconds M4  = 240
candlestickGranularityInSeconds M5  = 300
candlestickGranularityInSeconds M10 = 600
candlestickGranularityInSeconds M15 = 900
candlestickGranularityInSeconds M30 = 1800
candlestickGranularityInSeconds H1  = 3600
candlestickGranularityInSeconds H2  = 7200
candlestickGranularityInSeconds H3  = 10800
candlestickGranularityInSeconds H4  = 14400
candlestickGranularityInSeconds H6  = 21600
candlestickGranularityInSeconds H8  = 28800
candlestickGranularityInSeconds H12 = 43200
candlestickGranularityInSeconds D   = 86400
candlestickGranularityInSeconds W   = 604800
candlestickGranularityInSeconds M   = 2551442.976


candlestickGranularityToNomialDiffTime :: CandlestickGranularity -> NominalDiffTime
candlestickGranularityToNomialDiffTime = fromRational . toRational . candlestickGranularityInSeconds
