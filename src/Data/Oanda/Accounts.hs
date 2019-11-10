{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.Accounts
    ( Accounts (..)

    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics

import           Data.Oanda.AccountProperties

newtype Accounts = Accounts
  { accounts :: [AccountProperties]
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)
