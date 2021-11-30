{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.AccountDetailsGET
  ( GetAccountDetails(..)
  ) where

import           ApiMaker
import           Control.Monad.Trans

import           Data.Oanda.AccountDetails
import           Data.Oanda.Types
import           Request.Class


newtype GetAccountDetails = GetAccountDetails AccountId

instance Request OandaConfig GetAccountDetails where
  type Method GetAccountDetails = GET
  type Body GetAccountDetails = NoReqBody
  type Response GetAccountDetails = JsonResponse AccountDetails
  type Output GetAccountDetails = AccountDetails
  method _ GetAccountDetails {} = GET
  url cfg (GetAccountDetails id) = baseUrl cfg /: "accounts" /: id
  body _ GetAccountDetails {} = NoReqBody
  response _ GetAccountDetails {} = jsonResponse
  option _ GetAccountDetails {} = return headerRFC3339DatetimeFormat
  process _ GetAccountDetails {} response = return $ responseBody response
