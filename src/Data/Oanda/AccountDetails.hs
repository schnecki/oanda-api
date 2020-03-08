{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.AccountDetails
  ( AccountDetails(..)
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Text
import           Data.Time
import           GHC.Generics

import           Data.Oanda.Account hiding (lastTransactionID)
import           Data.Oanda.Types

data AccountDetails = AccountDetails
  { account           :: Account       -- ^ The full details of the requested Account.
  , lastTransactionID :: TransactionId -- ^ The ID of the most recent Transaction created for the Account.
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)
