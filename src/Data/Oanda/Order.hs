{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.Order
    ( Order (..)

    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Text
import           Data.Time
import           GHC.Generics

import           Data.Oanda.ClientExtensions
import           Data.Oanda.DateTime
import           Data.Oanda.OrderState
import           Data.Oanda.Types

data Order = Order
  { id               :: OrderId          -- ^ The Order’s identifier, unique within the Order’s Account.
  , createTime       :: DateTime          -- ^ The time when the Order was created.
  , state            :: OrderState       -- ^ The current state of the Order.
  , clientExtensions :: ClientExtensions -- ^ The client extensions of the Order. Do not set, modify, or delete
                                         -- clientExtensions if your account is associated with MT4.
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)
