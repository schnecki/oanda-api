{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Oanda.OrderPOST
  ( PostOrder (..)
  ) where

import           ApiMaker
import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics

import           Request.Oanda.Class

import           Data.Oanda.OrderRequest
import           Data.Oanda.OrderRequestResult
import           Data.Oanda.Types


data PostOrder =
  PostOrder AccountId
            OrderRequest

newtype OrderPOST = OrderPOST
  { order :: OrderRequest
  } deriving (Show, Eq, Ord, ToJSON, Generic, NFData)


instance Request OandaConfig PostOrder where
  type Method PostOrder = POST
  type Body PostOrder = ReqBodyJson OrderPOST
  type Response PostOrder = JsonResponse OrderRequestResult
  type Output PostOrder = OrderRequestResult
  method _ PostOrder {} = POST
  url cfg (PostOrder accId _) = baseUrl cfg /: "accounts" /: accId /: "orders"
  body _ (PostOrder _ req) = ReqBodyJson (OrderPOST req)
  response _ PostOrder {} = jsonResponse
  option _ PostOrder {} = return mempty
  process _ PostOrder {} response = return $ responseBody response
