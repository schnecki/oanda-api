{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Oanda.InstrumentsGET
  ( GetInstruments(..)
  ) where

import           ApiMaker
import qualified Data.Text              as T

import           Data.Oanda.Instruments
import           Data.Oanda.Types
import           Request.Oanda.Class


data GetInstruments = GetInstruments AccountId (Maybe [InstrumentName])

instance Request OandaConfig GetInstruments where
  type Method GetInstruments = GET
  type Body GetInstruments = NoReqBody
  type Response GetInstruments = JsonResponse Instruments
  type Output GetInstruments = Instruments
  method _ GetInstruments {} = GET
  url cfg (GetInstruments id _) = baseUrl cfg /: "accounts" /: id /: "instruments"
  body _ GetInstruments {} = NoReqBody
  response _ GetInstruments {} = jsonResponse
  option _ (GetInstruments _ mInsts) = return $
    headerRFC3339DatetimeFormat <>
    case mInsts of
      Nothing    -> mempty
      Just insts -> "instruments" =: T.intercalate "," insts
  process _ GetInstruments {} response = return $ responseBody response
