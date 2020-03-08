{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.OrderRequestResult
    ( OrderRequestResult (..)

    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics

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


