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
    , prettyTransaction
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text
import qualified Data.Text           as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.DateTime
import           Data.Oanda.Types


data Transaction = Transaction
  { id        :: TransactionId -- ^ The Transaction’s Identifier.
  , time      :: DateTime      -- ^ The date/time when the Transaction was created.
  , userID    :: Int           -- ^ The ID of the user that initiated the creation of the Transaction.
  , accountID :: AccountId     -- ^ The ID of the Account the Transaction was created for.
  , batchID   :: TransactionId -- ^ The ID of the “batch” that the Transaction belongs to. Transactions in     the same batch are applied to the Account simultaneously.
  , requestID :: RequestId     -- ^ The Request ID of the request which generated the transaction.
  } deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, NFData)


prettyTransaction :: Transaction -> Doc
prettyTransaction trans =
  colName "id"          $$ nest nestCols (text $ T.unpack $ Data.Oanda.Transaction.id trans)   $+$    -- TransactionId
  colName "time"        $$ nest nestCols (text $ show $ time trans)   $+$    -- DateTime
  colName "userID"      $$ nest nestCols (text $ show $ userID trans)   $+$    -- Int
  colName "accountID"   $$ nest nestCols (text $ T.unpack $ accountID trans)   $+$    -- AccountId
  colName "batchID"     $$ nest nestCols (text $ T.unpack $ batchID trans)   $+$    -- TransactionId
  colName "requestID"   $$ nest nestCols (text $ T.unpack $ requestID trans)     -- RequestId
