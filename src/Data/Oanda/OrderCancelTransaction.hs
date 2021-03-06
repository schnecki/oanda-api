{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Missing implementations: OrderCancelTransaction, OrderCancelTransaction, OrderCancelRejectTransaction,
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
module Data.Oanda.OrderCancelTransaction
    ( OrderCancelTransaction (..)
    , prettyOrderCancelTransaction
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text                    as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.DateTime
import           Data.Oanda.OrderCancelReason
import           Data.Oanda.TransactionType
import           Data.Oanda.Types


data OrderCancelTransaction = OrderCancelTransaction
  { id                :: TransactionId     -- ^ The Transaction’s Identifier.
  , time              :: DateTime          -- ^ The date/time when the Transaction was created.
  , userID            :: Int               -- ^ The ID of the user that initiated the creation of the Transaction.
  , accountID         :: AccountId         -- ^ The ID of the Account the Transaction was created for.
  , batchID           :: TransactionId     -- ^ The ID of the “batch” that the Transaction belongs to. Transactions in     the same batch are applied to the Account simultaneously.
  , requestID         :: RequestId         -- ^ The Request ID of the request which generated the transaction.
  , transactionType   :: TransactionType   -- ^ The Type of the Transaction. Always set to “ORDER_CANCEL” for an     OrderCancelTransaction. [default=ORDER_CANCEL]
  , orderID           :: OrderId           -- ^ The ID of the Order cancelled
  , clientOrderID     :: Maybe OrderId           -- ^ The client ID of the Order cancelled (only provided if the Order has a client Order ID.
  , reason            :: OrderCancelReason -- ^ The reason that the Order was cancelled.
  , replacedByOrderID :: Maybe OrderId           -- ^ The ID of the Order that replaced this Order (only provided if this Order     was cancelled for replacement).
  } deriving (Generic, Show, Eq, Ord, ToJSON, NFData)

-- Derive from JSON by modifying the NAV-nav field.
$(deriveFromJSON
    defaultOptions
      { fieldLabelModifier =
          let f "transactionType" = "type"
              f x                 = x
           in f
      }
    ''OrderCancelTransaction)


prettyOrderCancelTransaction :: OrderCancelTransaction -> Doc
prettyOrderCancelTransaction trans =
  colName "id"                                                      $$ nest nestCols (text $ T.unpack $ Data.Oanda.OrderCancelTransaction.id trans)  $+$ -- TransactionId
  colName "time"                                                    $$ nest nestCols (text $ show $ time trans)  $+$                                     -- DateTime
  colName "userID"                                                  $$ nest nestCols (text $ show $ userID trans)  $+$                                   -- Int
  colName "accountID"                                               $$ nest nestCols (text $ T.unpack $ accountID trans)  $+$                            -- AccountId
  colName "batchID"                                                 $$ nest nestCols (text $ T.unpack $ batchID trans)  $+$                              -- TransactionId
  colName "requestID"                                               $$ nest nestCols (text $ T.unpack $ requestID trans)  $+$                            -- RequestId
  colName "transactionType"                                         $$ nest nestCols (text $ show $ transactionType trans)  $+$                          -- TransactionType
  colName "orderID"                                                 $$ nest nestCols (text $ T.unpack $ orderID trans)  $+$                              -- OrderId
  mVal (clientOrderID trans) (\v-> colName "clientOrderID"          $$ nest nestCols (text $ T.unpack v))  $+$                                           -- Maybe OrderId
  colName "reason"                                                  $$ nest nestCols (text $ show $ reason trans)  $+$                                   -- OrderCancelReason
  mVal (replacedByOrderID trans) (\v -> colName "replacedByOrderID" $$ nest nestCols (text $ T.unpack v))                                                -- Maybe OrderId
