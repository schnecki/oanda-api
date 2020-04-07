{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.OpenTradesGET
  ( GetOpenTrades (..)
  ) where

import           ApiMaker

import           Data.Oanda.TradeList
import           Data.Oanda.Types
import           Request.Class


newtype GetOpenTrades =
  GetOpenTrades AccountId


instance Request OandaConfig GetOpenTrades where
  type Method GetOpenTrades = GET
  type Body GetOpenTrades = NoReqBody
  type Response GetOpenTrades = JsonResponse TradeList
  type Output GetOpenTrades = TradeList
  method _ GetOpenTrades {} = GET
  url cfg (GetOpenTrades accId) = baseUrl cfg /: "accounts" /: accId /: "openTrades"
  body _ GetOpenTrades {} = NoReqBody
  response _ GetOpenTrades {} = jsonResponse
  option _ GetOpenTrades {} = headerRFC3339DatetimeFormat
  process _ GetOpenTrades {} response = return $ responseBody response
