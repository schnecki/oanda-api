{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Oanda.AccountConfigurationResult
    ( AccountConfigurationResult (..)
    , accountConfigurationResultSuccess
    , prettyAccountConfigurationResult
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Maybe                            (isNothing)
import           Data.Text
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.ClientConfigureTransaction
import           Data.Oanda.Types


data AccountConfigurationResult = AccountConfigurationResult
  { clientConfigureTransaction :: ClientConfigureTransaction -- ^ The transaction that configures the Account.
  , lastTransactionID          :: TransactionId              -- ^ The ID of the last Transaction created for the Account.
  , errorCode                  :: Maybe Text                 -- ^ The code of the error that has occurred. This field may not be returned for some errors.
  , errorMessage               :: Maybe Text                 -- ^ The human-readable description of the error that has occurred.
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)

-- makeLenses ''AccountConfigurationResult

-- deriveJSON (unPrefix "_accountConfigRes") ''AccountConfigurationResult


prettyAccountConfigurationResult :: AccountConfigurationResult -> Doc
prettyAccountConfigurationResult res =
  colName "clientConfigureTransaction" $$ nest nestCols (text $ show $ clientConfigureTransaction res) $+$
  colName "lastTransactionID"          $$ nest nestCols (text $ show $ lastTransactionID res) $+$
  colName "errorCode"                  $$ nest nestCols (text $ show $ errorCode res) $+$
  colName "errorMessage"               $$ nest nestCols (text $ show $ errorMessage res)

-- | Returns true if the configuration was successful, i.e. if no error code was returned.
accountConfigurationResultSuccess :: AccountConfigurationResult -> Bool
accountConfigurationResultSuccess = isNothing . errorCode
