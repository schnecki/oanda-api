{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.Candles
    ( Candles
    , prettyCandles
    ) where

import           Control.DeepSeq
import           Data.Aeson
import qualified Data.Text                         as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.Candlestick
import           Data.Oanda.CandlestickGranularity
import           Data.Oanda.Types

data Candles = Candles
  { instrument  :: InstrumentName         -- ^ The instrument whose Prices are represented by the candlesticks.
  , granularity :: CandlestickGranularity -- ^ The granularity of the candlesticks provided.
  , candles     :: [Candlestick]          -- ^ The list of candlesticks that satisfy the request.
  } deriving (Show, Eq, Ord, Generic, FromJSON, NFData)


prettyCandles :: Candles -> Doc
prettyCandles cdls =
  colName "instrument"      $$ nest nestCols (text $ T.unpack $ instrument cdls) $+$
  colName "granularity"     $$ nest nestCols (text $ show $ granularity cdls)    $+$
  colName "candles"         $$ nest nestCols (vcat $ map (prettyCandlestickWith 10) $ candles cdls)

