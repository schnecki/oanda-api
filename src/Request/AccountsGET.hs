{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.AccountsGET
  ( GetAccounts (..)
  ) where

import           ApiMaker

import           Data.Oanda.Accounts
import           Request.Class

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
  option _ GetAccounts = mempty
  process _ GetAccounts response = return $ responseBody response

