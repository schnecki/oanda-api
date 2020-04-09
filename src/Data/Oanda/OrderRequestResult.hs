{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.OrderRequestResult
    ( OrderRequestResult (..)
    , prettyOrderRequestResult
    ) where

import           Control.DeepSeq
import           Data.Aeson
import qualified Data.Text                         as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.OrderCancelTransaction
import           Data.Oanda.OrderFillTransaction
import           Data.Oanda.Transaction
import           Data.Oanda.Types


data OrderRequestResult = OrderRequestResult
  { orderCreateTransaction        :: Transaction                  -- ^ The Transaction that created the Order specified by the request.
  , orderFillTransaction          :: Maybe OrderFillTransaction   -- ^ The Transaction that filled the newly created Order. Only provided when the Order was immediately filled.
  , orderCancelTransaction        :: Maybe OrderCancelTransaction -- ^ The Transaction that cancelled the newly created Order. Only provided when the Order was immediately cancelled.
  , orderReissueTransaction       :: Maybe Transaction            -- ^ The Transaction that reissues the Order. Only provided when the Order is configured to be reissued for its remaining units after a partial fill     and the reissue was successful.
  , orderReissueRejectTransaction :: Maybe Transaction            -- ^ The Transaction that rejects the reissue of the Order. Only provided when the Order is configured to be reissued for its remaining units after a     partial fill and the reissue was rejected.
  , relatedTransactionIDs         :: [TransactionId]              -- ^ The IDs of all Transactions that were created while satisfying the request.
  , lastTransactionID             :: TransactionId                -- ^ The ID of the most recent Transaction created for the Account
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)


prettyOrderRequestResult :: OrderRequestResult -> Doc
prettyOrderRequestResult res =
  colName "orderCreateTransaction"                                                        $$ nest nestCols (prettyTransaction $ orderCreateTransaction res) $+$ -- Transaction
  mVal (orderFillTransaction res) (\v -> colName "orderFillTransaction"                   $$ nest nestCols (prettyOrderFillTransaction v)) $+$                  -- Maybe OrderFillTransaction
  mVal (orderCancelTransaction res) (\v -> colName "orderCancelTransaction"               $$ nest nestCols (prettyOrderCancelTransaction v)) $+$                -- Maybe OrderCancelTransaction
  mVal (orderReissueTransaction res) (\v -> colName "orderReissueTransaction"             $$ nest nestCols (prettyTransaction v)) $+$                           -- Maybe Transaction
  mVal (orderReissueRejectTransaction res) (\v -> colName "orderReissueRejectTransaction" $$ nest nestCols (prettyTransaction v)) $+$                           -- Maybe Transaction
  colName "relatedTransactionIDs"                                                         $$ nest nestCols (text $ show $ relatedTransactionIDs res) $+$        -- [TransactionId]
  colName "lastTransactionID"                                                             $$ nest nestCols (text $ T.unpack $ lastTransactionID res)            -- TransactionId

