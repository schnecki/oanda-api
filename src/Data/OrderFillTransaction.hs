{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

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
module Data.OrderFillTransaction
    ( OrderFillTransaction (..)
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text
import           GHC.Generics

import           Data.AccountUnits
import           Data.ClientPrice
import           Data.DateTime
import           Data.DecimalNumber
import           Data.OrderFillReason
import           Data.TradeOpen
import           Data.TradeReduce
import           Data.TransactionType
import           Types


data OrderFillTransaction = OrderFillTransaction
  { id                            :: TransactionId -- ^ The Transaction’s Identifier.
  , time                          :: DateTime -- ^ The date/time when the Transaction was created.
  , userID                        :: Int -- ^ The ID of the user that initiated the creation of the Transaction.
  , accountID                     :: AccountId -- ^ The ID of the Account the Transaction was created for.
  , batchID                       :: TransactionId -- ^ The ID of the “batch” that the Transaction belongs to. Transactions in     the same batch are applied to the Account simultaneously.
  , requestID                     :: RequestId -- ^ The Request ID of the request which generated the transaction.
  , transactionType               :: TransactionType -- ^ The Type of the Transaction. Always set to “ORDER_FILL” for an OrderFillTransaction. [default=ORDER_FILL]
  , orderID                       :: OrderId -- ^ The ID of the Order filled.
  , clientOrderID                 :: Maybe ClientId -- ^ The client Order ID of the Order filled (only provided if the client has assigned one).
  , instrument                    :: InstrumentName -- ^ The name of the filled Order’s instrument.
  , units                         :: DecimalNumber -- ^ The number of units filled by the Order.
  , gainQuoteHomeConversionFactor :: DecimalNumber -- ^ This is the conversion factor in effect for the Account at the time of the OrderFill for converting any gains realized in Instrument quote units     into units of the Account’s home currency.
  , lossQuoteHomeConversionFactor :: DecimalNumber -- ^ This is the conversion factor in effect for the Account at the time of the OrderFill for converting any losses realized in Instrument quote     units into units of the Account’s home currency.
  , fullPrice                     :: ClientPrice -- ^ The price in effect for the account at the time of the Order fill.
  , reason                        :: OrderFillReason -- ^ The reason that an Order was filled
  , pl                            :: AccountUnits -- ^ The profit or loss incurred when the Order was filled.
  , financing                     :: AccountUnits -- ^ The financing paid or collected when the Order was filled.
  , commission                    :: AccountUnits -- ^ The commission charged in the Account’s home currency as a result of filling the Order. The commission is always represented as a positive     quantity of the Account’s home currency, however it reduces the balance     in the Account.
  , guaranteedExecutionFee        :: AccountUnits -- ^ The total guaranteed execution fees charged for all Trades opened, closed or reduced with guaranteed Stop Loss Orders.
  , accountBalance                :: AccountUnits -- ^ The Account’s balance after the Order was filled.
  , tradeOpened                   :: Maybe TradeOpen -- ^ The Trade that was opened when the Order was filled (only provided if     filling the Order resulted in a new Trade).
  , tradesClosed                  :: [TradeReduce] -- ^ The Trades that were closed when the Order was filled (only provided if     filling the Order resulted in a closing open Trades).
  , tradeReduced                  :: Maybe TradeReduce -- ^ The Trade that was reduced when the Order was filled (only provided if     filling the Order resulted in reducing an open Trade).
  , halfSpreadCost                :: AccountUnits -- ^ The half spread cost for the OrderFill, which is the sum of the     halfSpreadCost values in the tradeOpened, tradesClosed and tradeReduced     fields. This can be a positive or negative value and is represented in     the home currency of the Account.


  } deriving (Generic, Show, Eq, Ord, ToJSON, NFData)

-- Derive from JSON by modifying the NAV-nav field.
$(deriveFromJSON
    defaultOptions
      { fieldLabelModifier =
          let f "transactionType" = "type"
              f x                 = x
           in f
      }
    ''OrderFillTransaction)

