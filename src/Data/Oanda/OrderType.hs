{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module Data.Oanda.OrderType
    ( OrderType (..)
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics


data OrderType
  = MARKET
  | LIMIT
  | STOP
  | MARKET_IF_TOUCHED
  | TAKE_PROFIT
  | STOP_LOSS
  | TRAILING_STOP_LOSS
  deriving (Enum, Bounded, Show, Eq, Ord, ToJSON, FromJSON, Generic, NFData)
