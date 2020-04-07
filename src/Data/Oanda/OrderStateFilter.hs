{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.OrderStateFilter
    ( OrderStateFilter (..)
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics


data OrderStateFilter
  = PENDING   -- ^ The Orders that are currently pending execution
  | FILLED    -- ^ The Orders that have been filled
  | TRIGGERED -- ^ The Orders that have been triggered
  | CANCELLED -- ^ The Orders that have been cancelled
  | ALL       -- ^ The Orders that are in any of the possible states listed above
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, NFData)
