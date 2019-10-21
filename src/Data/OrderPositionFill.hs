{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module Data.OrderPositionFill
    ( OrderPositionFill (..)
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics


data OrderPositionFill
  = OPEN_ONLY    -- ^ When the Order is filled, only allow Positions to be opened or extended.
  | REDUCE_FIRST -- ^ When the Order is filled, always fully reduce an existing Position before opening a new Position.
  | REDUCE_ONLY  -- ^ When the Order is filled, only reduce an existing Position.
  | DEFAULT      -- ^ When the Order is filled, use REDUCE_FIRST behaviour for non-client hedging Accounts, and OPEN_ONLY behaviour for client hedging Accounts.
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Generic, NFData)
