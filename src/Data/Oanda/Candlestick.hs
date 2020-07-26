{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Oanda.Candlestick
    ( Candlestick (..)
    , prettyCandlestick
    , prettyCandlestickWith
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.CandlestickData
import           Data.Oanda.DateTime
import           Data.Oanda.Types

data Candlestick = Candlestick
  { time     :: DateTime              -- ^ The start time of the candlestick
  , bid      :: Maybe CandlestickData -- ^ The candlestick data based on bids. Only provided if bid-based candles were requested.
  , ask      :: Maybe CandlestickData -- ^ The candlestick data based on asks. Only provided if ask-based candles were requested.
  , mid      :: Maybe CandlestickData -- ^ The candlestick data based on midpoints. Only provided if midpoint-based candles were requested.
  , volume   :: Int                   -- ^ The number of prices created during the time-range represented by the candlestick.
  , complete :: Bool                  -- ^ A flag indicating if the candlestick is complete. A complete candlestick is one whose ending time is not in the future.
  } deriving (Show, Read, Eq, Ord, Serialize, Generic, FromJSON, NFData)


prettyCandlestick :: Candlestick -> Doc
prettyCandlestick = prettyCandlestickWith nestCols

prettyCandlestickWith :: Int -> Candlestick -> Doc
prettyCandlestickWith nesting stick =
  colName "time"     $$ nest nesting (text $ show $ time stick) $+$
  mVal (bid stick) (\v -> colName "bid"      $$ nest nesting (prettyCandlestickData v)) $+$
  mVal (ask stick) (\v -> colName "ask"      $$ nest nesting (prettyCandlestickData v)) $+$
  mVal (mid stick) (\v -> colName "mid"      $$ nest nesting (prettyCandlestickData v)) $+$
  colName "volume"   $$ nest nesting (text $ show $ volume stick) $+$
  colName "complete" $$ nest nesting (text $ show $ complete stick)
