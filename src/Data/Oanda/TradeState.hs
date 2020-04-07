{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.TradeState
  ( TradeState(..)
  ) where


import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics


data TradeState
  = OPEN                 -- ^ The Trade is currently open
  | CLOSED               -- ^ The Trade has been fully closed
  | CLOSE_WHEN_TRADEABLE -- ^ The Trade will be closed as soon as the trade’s instrument becomes tradeable
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Generic, NFData)
