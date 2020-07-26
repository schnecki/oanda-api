{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.CandlestickData
    ( CandlestickData (..)
    , prettyCandlestickData
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import qualified Data.Text             as T
import           GHC.Generics
import           Prelude               hiding ((<>))
import           Text.PrettyPrint

import           Data.Oanda.PriceValue
import           Data.Oanda.Types

data CandlestickData = CandlestickData
  { o :: PriceValue             -- ^ The first (open) price in the time-range represented by the candlestick.
  , h :: PriceValue             -- ^ The highest price in the time-range represented by the candlestick.
  , l :: PriceValue             -- ^ The lowest price in the time-range represented by the candlestick.
  , c :: PriceValue             -- ^ The last (closing) price in the time-range represented by the candlestick.
  } deriving (Show, Read, Eq, Ord, Serialize, Generic, ToJSON, FromJSON, NFData)


prettyCandlestickData :: CandlestickData -> Doc
prettyCandlestickData candle =
  colName "open"    <+> prettyPriceValue (o candle) <> text ";" <+>
  colName "highest" <+> prettyPriceValue (h candle) <> text ";" <+>
  colName "lowest"  <+> prettyPriceValue (l candle) <> text ";" <+>
  colName "closing" <+> prettyPriceValue (c candle)
