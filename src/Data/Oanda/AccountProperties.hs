{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.AccountProperties
    ( AccountProperties (..)
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Text
import           GHC.Generics

import           Data.Oanda.Types

data AccountProperties = AccountProperties
  { id           :: AccountId
  , mt4AccountID :: Maybe Int
  , tags         :: [Text]
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)
