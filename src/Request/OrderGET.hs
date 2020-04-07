{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.OrderGET
  ( GetOrder (..)
  , maxOrders
  , last50Orders
  ) where

import           ApiMaker
import           Control.DeepSeq
import           Data.Aeson
import qualified Data.Text                     as T
import           GHC.Generics

import           Request.Class

import           Data.Oanda.OrderList
import           Data.Oanda.OrderRequest
import           Data.Oanda.OrderRequestResult
import           Data.Oanda.OrderStateFilter
import           Data.Oanda.Types


data GetOrder =
  GetOrder AccountId OrderConfig

data OrderConfig = OrderConfig
  { ids        :: Maybe [OrderId]           -- ^ List of Order IDs to retrieve
  , state      :: Maybe OrderStateFilter    -- ^ The state to filter the requested Orders by [default=PENDING]
  , instrument :: Maybe InstrumentName      -- ^ The instrument to filter the requested orders by
  , count      :: Maybe Int                 -- ^ The maximum number of Orders to return [default=50, maximum=500]
  , beforeID   :: Maybe OrderId             -- ^ The maximum Order ID to return. If not provided the most recent Orders in the Account are returned
  } deriving (Show, Eq, Ord, ToJSON, Generic, NFData)

maxOrders, last50Orders :: OrderConfig
maxOrders = OrderConfig Nothing Nothing Nothing (Just 500) Nothing
last50Orders = OrderConfig Nothing Nothing Nothing (Just 50) Nothing

instance Request OandaConfig GetOrder where
  type Method GetOrder = GET
  type Body GetOrder = NoReqBody
  type Response GetOrder = JsonResponse OrderList
  type Output GetOrder = OrderList
  method _ GetOrder {} = GET
  url cfg (GetOrder accId _) = baseUrl cfg /: "accounts" /: accId /: "orders"
  body _ (GetOrder _ req) = NoReqBody
  response _ GetOrder {} = jsonResponse
  option _ (GetOrder _ cfg) = headerRFC3339DatetimeFormat <> configs
    where
      configs =
        case cfg of
          OrderConfig ids state instrument count beforeID ->
            "ids"        `queryParam` fmap (T.intercalate ";") ids <>
            "state"      `queryParam` fmap show state <>
            "instrument" `queryParam` instrument <>
            "count"      `queryParam` count <>
            "beforeID"   `queryParam` beforeID

  process _ GetOrder {} response = return $ responseBody response


