{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- | Missing implementations: OrderFillTransaction, OrderCancelTransaction, OrderCancelRejectTransaction,
-- OrderClientExtensionsModifyTransaction, OrderClientExtensionsModifyRejectTransaction, CreateTransaction,
-- CloseTransaction, ReopenTransaction, ClientConfigureTransaction, ClientConfigureRejectTransaction,
-- TransferFundsTransaction, TransferFundsRejectTransaction, MarketOrderTransaction, MarketOrderRejectTransaction,
-- FixedPriceOrderTransaction, LimitOrderTransaction, LimitOrderRejectTransaction, StopOrderTransaction,
-- StopOrderRejectTransaction, MarketIfTouchedOrderTransaction, MarketIfTouchedOrderRejectTransaction,
-- TakeProfitOrderTransaction, TakeProfitOrderRejectTransaction, StopLossOrderTransaction,
-- StopLossOrderRejectTransaction, TrailingStopLossOrderTransaction, TrailingStopLossOrderRejectTransaction,
-- TradeClientExtensionsModifyTransaction, TradeClientExtensionsModifyRejectTransaction, MarginCallEnterTransaction,
-- MarginCallExtendTransaction, MarginCallExitTransaction, DelayedTradeClosureTransaction, DailyFinancingTransaction,
-- ResetResettablePLTransaction
module Data.Oanda.Transaction
    ( Transaction (..)
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text
import           GHC.Generics

import           Data.Oanda.DateTime
import           Types


data Transaction = Transaction
  { id        :: TransactionId -- ^ The Transaction’s Identifier.
  , time      :: DateTime      -- ^ The date/time when the Transaction was created.
  , userID    :: Int           -- ^ The ID of the user that initiated the creation of the Transaction.
  , accountID :: AccountId     -- ^ The ID of the Account the Transaction was created for.
  , batchID   :: TransactionId -- ^ The ID of the “batch” that the Transaction belongs to. Transactions in     the same batch are applied to the Account simultaneously.
  , requestID :: RequestId     -- ^ The Request ID of the request which generated the transaction.
  } deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, NFData)
