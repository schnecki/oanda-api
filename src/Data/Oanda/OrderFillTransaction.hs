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
module Data.Oanda.OrderFillTransaction
    ( OrderFillTransaction (..)
    , prettyOrderFillTransaction
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text                  as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.AccountUnits
import           Data.Oanda.ClientPrice
import           Data.Oanda.DateTime
import           Data.Oanda.DecimalNumber
import           Data.Oanda.OrderFillReason
import           Data.Oanda.TradeOpen
import           Data.Oanda.TradeReduce
import           Data.Oanda.TransactionType
import           Data.Oanda.Types


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

prettyOrderFillTransaction :: OrderFillTransaction -> Doc
prettyOrderFillTransaction order =
  colName "id"                                              $$ nest nestCols (text $ T.unpack $ Data.Oanda.OrderFillTransaction.id order) $+$                 -- TransactionId
  colName "time"                                            $$ nest nestCols (text $ show $ time order) $+$                                                   -- DateTime
  colName "userID"                                          $$ nest nestCols (text $ show $ userID order) $+$                                                 -- Int
  colName "accountID"                                       $$ nest nestCols (text $ T.unpack $ accountID order) $+$                                          -- AccountId
  colName "batchID"                                         $$ nest nestCols (text $ T.unpack $ batchID order) $+$                                            -- TransactionId
  colName "requestID"                                       $$ nest nestCols (text $ T.unpack $ requestID order) $+$                                          -- RequestId
  colName "transactionType"                                 $$ nest nestCols (text $ show $ transactionType order) $+$                                        -- TransactionType
  colName "orderID"                                         $$ nest nestCols (text $ T.unpack $ orderID order) $+$                                            -- OrderId
  mVal (clientOrderID order) (\v -> colName "clientOrderID" $$ nest nestCols (text $ T.unpack v)) $+$                                                         -- Maybe ClientId
  colName "instrument"                                      $$ nest nestCols (text $ T.unpack $ instrument order) $+$                                         -- InstrumentName
  colName "units"                                           $$ nest nestCols (text $ show $ Data.Oanda.OrderFillTransaction.units order) $+$                  -- DecimalNumber
  colName "gainQuoteHomeConversionFactor"                   $$ nest nestCols (text $ show $ gainQuoteHomeConversionFactor order) $+$                          -- DecimalNumber
  colName "lossQuoteHomeConversionFactor"                   $$ nest nestCols (text $ show $ lossQuoteHomeConversionFactor order) $+$                          -- DecimalNumber
  colName "fullPrice"                                       $$ nest nestCols (text $ show $ fullPrice order) $+$                                              -- ClientPrice
  colName "reason"                                          $$ nest nestCols (text $ show $ reason order) $+$                                                 -- OrderFillReason
  colName "pl"                                              $$ nest nestCols (text $ show $ pl order) $+$                                                     -- AccountUnits
  colName "financing"                                       $$ nest nestCols (text $ show $ Data.Oanda.OrderFillTransaction.financing order) $+$              -- AccountUnits
  colName "commission"                                      $$ nest nestCols (text $ show $ commission order) $+$                                             -- AccountUnits
  colName "guaranteedExecutionFee"                          $$ nest nestCols (text $ show $ Data.Oanda.OrderFillTransaction.guaranteedExecutionFee order) $+$ -- AccountUnits
  colName "accountBalance"                                  $$ nest nestCols (text $ show $ accountBalance order) $+$                                         -- AccountUnits
  mVal (tradeOpened order) (\v -> colName "tradeOpened"     $$ nest nestCols (prettyTradeOpen v)) $+$                                                         -- Maybe TradeOpen
  colName "tradesClosed"                                    $$ nest nestCols (vcat $ Prelude.map prettyTradeReduce $ tradesClosed order) $+$                  -- [TradeReduce]
  mVal (tradeReduced order) (\v -> colName "tradeReduced"   $$ nest nestCols (prettyTradeReduce v)) $+$                                                       -- Maybe TradeReduce
  colName "halfSpreadCost"                                  $$ nest nestCols (text $ show $ Data.Oanda.OrderFillTransaction.halfSpreadCost order)             -- AccountUnits
