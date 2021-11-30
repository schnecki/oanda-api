{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.AccountConfigurationPATCH
  ( PatchAccountConfiguration(..)
  , AccountConfigurationUpdate (..)
  ) where

import           ApiMaker
import           Data.Aeson
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           GHC.Generics

import           Data.Oanda.AccountConfigurationResult
import           Data.Oanda.DecimalNumber
import           Data.Oanda.Types
import           Request.Class

data PatchAccountConfiguration = PatchAccountConfiguration AccountId AccountConfigurationUpdate

data AccountConfigurationUpdate = AccountConfigurationUpdate
  { alias      :: Maybe Text          -- ^ Client-defined alias (name) for the Account
  , marginRate :: Maybe DecimalNumber -- ^ Margin rate to set. Allowed values in the Webinterface: 0.01, 0.02, 0.025,
                                      -- 0.033, 0.05, 0.1. Others might not work.
  } deriving (Eq, Ord, Show, Generic, ToJSON)


instance Request OandaConfig PatchAccountConfiguration where
  type Method PatchAccountConfiguration = PATCH
  type Body PatchAccountConfiguration = ReqBodyJson AccountConfigurationUpdate
  type Response PatchAccountConfiguration = JsonResponse AccountConfigurationResult
  type Output PatchAccountConfiguration = AccountConfigurationResult
  method _ PatchAccountConfiguration {} = PATCH
  url cfg (PatchAccountConfiguration accId _)  = baseUrl cfg /: "accounts" /: accId /: "configuration"
  body _ (PatchAccountConfiguration _ config) = ReqBodyJson config
  response _ PatchAccountConfiguration {} = jsonResponse
  option _ PatchAccountConfiguration {} = return headerRFC3339DatetimeFormat
  process _ PatchAccountConfiguration {} response = return $ responseBody response
