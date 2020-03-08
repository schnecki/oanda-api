{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.AccountSummaryGET
  ( GetAccountSummary(..)
  ) where

import           ApiMaker

import           Data.Oanda.AccountDetails
import           Data.Oanda.Types
import           Request.Class


newtype GetAccountSummary = GetAccountSummary AccountId

instance Request OandaConfig GetAccountSummary where
  type Method GetAccountSummary = GET
  type Body GetAccountSummary = NoReqBody
  type Response GetAccountSummary = JsonResponse AccountDetails
  type Output GetAccountSummary = AccountDetails
  method _ GetAccountSummary {} = GET
  url cfg (GetAccountSummary id) = baseUrl cfg /: "accounts" /: id
  body _ GetAccountSummary {} = NoReqBody
  response _ GetAccountSummary {} = jsonResponse
  option _ GetAccountSummary {} = headerRFC3339DatetimeFormat
  process _ GetAccountSummary {} response = return $ responseBody response
