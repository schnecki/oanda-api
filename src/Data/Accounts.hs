{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Accounts
    ( Accounts (..)

    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics

import           Data.AccountProperties

newtype Accounts = Accounts
  { accounts :: [AccountProperties]
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)
