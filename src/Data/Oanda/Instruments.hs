{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.Instruments
  ( Instruments(..)
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics

import           Data.Oanda.Instrument
import           Data.Oanda.Types

data Instruments = Instruments
  { instruments       :: [Instrument] -- ^ The requested list of instruments.
  , lastTransactionID :: TransactionId -- ^ The ID of the most recent Transaction created for the Account.
  } deriving (Generic, Show, Eq, Ord, FromJSON, NFData)
