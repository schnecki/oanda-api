{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.OrderState
    ( OrderState (..)

    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Text
import           Data.Time
import           GHC.Generics

import           Types


data OrderState
  = PENDING                     -- ^ The Order is currently pending execution
  | FILLED                      -- ^ The Order has been filled
  | TRIGGERED                   -- ^ The Order has been triggered
  | CANCELLED                   -- ^ The Order has been cancelled
  deriving (Show, Eq, Ord, FromJSON, Generic, NFData)
