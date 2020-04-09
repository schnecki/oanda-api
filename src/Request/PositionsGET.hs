{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.PositionsGET
  ( GetPositions (..)
  ) where

import           ApiMaker

import           Data.Oanda.PositionList
import           Data.Oanda.Types
import           Request.Class


newtype GetPositions =
  GetPositions AccountId


instance Request OandaConfig GetPositions where
  type Method GetPositions = GET
  type Body GetPositions = NoReqBody
  type Response GetPositions = JsonResponse PositionList
  type Output GetPositions = PositionList
  method _ GetPositions {} = GET
  url cfg (GetPositions accId) = baseUrl cfg /: "accounts" /: accId /: "positions"
  body _ GetPositions {} = NoReqBody
  response _ GetPositions {} = jsonResponse
  option _ GetPositions {} = headerRFC3339DatetimeFormat
  process _ GetPositions {} response = return $ responseBody response
