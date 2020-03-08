{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Oanda.AccountConfigurationResult
    ( AccountConfigurationResult (..)
    ) where

import           Control.DeepSeq
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH                         (deriveJSON)
import           Data.Text
import           GHC.Generics

import           Data.Oanda.ClientConfigureTransaction
import           Data.Oanda.Types
import           Util

data AccountConfigurationResult = AccountConfigurationResult
  { clientConfigureTransaction :: ClientConfigureTransaction -- ^ The transaction that configures the Account.
  , lastTransactionID          :: TransactionId              -- ^ The ID of the last Transaction created for the Account.
  , errorCode                  :: Maybe Text                 -- ^ The code of the error that has occurred. This field may not be returned for some errors.
  , errorMessage               :: Maybe Text                 -- ^ The human-readable description of the error that has occurred.
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)

-- makeLenses ''AccountConfigurationResult

-- deriveJSON (unPrefix "_accountConfigRes") ''AccountConfigurationResult


