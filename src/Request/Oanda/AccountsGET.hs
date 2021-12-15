{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Oanda.AccountsGET
  ( GetAccounts (..)
  ) where

import           ApiMaker

import           Data.Oanda.Accounts
import           Request.Oanda.Class

data GetAccounts =
  GetAccounts

instance Request OandaConfig GetAccounts where
  type Method GetAccounts = GET
  type Body GetAccounts = NoReqBody
  type Response GetAccounts = JsonResponse Accounts
  type Output GetAccounts = Accounts
  method _ GetAccounts = GET
  url cfg GetAccounts = baseUrl cfg /: "accounts"
  body _ GetAccounts = NoReqBody
  response _ GetAccounts = jsonResponse
  option _ GetAccounts = return mempty
  process _ GetAccounts response = return $ responseBody response

