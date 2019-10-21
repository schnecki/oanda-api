{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.TradeState
  ( TradeState(..)
  ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Text
import           Data.Time
import           GHC.Generics

import           Types


data TradeState
  = OPEN -- ^ The Trade is currently open
  | CLOSED -- ^ The Trade has been fully closed
  | CLOSE_WHEN_TRADEABLE -- ^ The Trade will be closed as soon as the trade’s instrument becomes tradeable
  deriving (Show, Eq, Ord, FromJSON, Generic, NFData)