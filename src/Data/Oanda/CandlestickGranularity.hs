{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.CandlestickGranularity
    ( CandlestickGranularity (..)
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


candlestickGranularityToNomialDiffTime :: CandlestickGranularity -> NominalDiffTime
candlestickGranularityToNomialDiffTime S5  = 5
candlestickGranularityToNomialDiffTime S10 = 10
candlestickGranularityToNomialDiffTime S15 = 15
candlestickGranularityToNomialDiffTime S30 = 30
candlestickGranularityToNomialDiffTime M1  = 60
candlestickGranularityToNomialDiffTime M2  = 120
candlestickGranularityToNomialDiffTime M4  = 240
candlestickGranularityToNomialDiffTime M5  = 300
candlestickGranularityToNomialDiffTime M10 = 600
candlestickGranularityToNomialDiffTime M15 = 900
candlestickGranularityToNomialDiffTime M30 = 1800
candlestickGranularityToNomialDiffTime H1  = 3600
candlestickGranularityToNomialDiffTime H2  = 7200
candlestickGranularityToNomialDiffTime H3  = 10800
candlestickGranularityToNomialDiffTime H4  = 14400
candlestickGranularityToNomialDiffTime H6  = 21600
candlestickGranularityToNomialDiffTime H8  = 28800
candlestickGranularityToNomialDiffTime H12 = 43200
candlestickGranularityToNomialDiffTime D   = 86400
candlestickGranularityToNomialDiffTime W   = 604800
candlestickGranularityToNomialDiffTime M   = 2551442.976
