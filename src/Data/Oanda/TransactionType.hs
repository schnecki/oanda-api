{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.TransactionType
    ( TransactionType (..)
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text
import           GHC.Generics

import           Data.Oanda.DateTime
import           Data.Oanda.Types


data TransactionType
  = CREATE                                -- ^ Account Create Transaction
  | CLOSE                                 -- ^ Account Close Transaction
  | REOPEN                                -- ^ Account Reopen Transaction
  | CLIENT_CONFIGURE                      -- ^ Client Configuration Transaction
  | CLIENT_CONFIGURE_REJECT               -- ^ Client Configuration Reject Transaction
  | TRANSFER_FUNDS                        -- ^ Transfer Funds Transaction
  | TRANSFER_FUNDS_REJECT                 -- ^ Transfer Funds Reject Transaction
  | MARKET_ORDER                          -- ^ Market Order Transaction
  | MARKET_ORDER_REJECT                   -- ^ Market Order Reject Transaction
  | FIXED_PRICE_ORDER                     -- ^ Fixed Price Order Transaction
  | LIMIT_ORDER                           -- ^ Limit Order Transaction
  | LIMIT_ORDER_REJECT                    -- ^ Limit Order Reject Transaction
  | STOP_ORDER                            -- ^ Stop Order Transaction
  | STOP_ORDER_REJECT                     -- ^ Stop Order Reject Transaction
  | MARKET_IF_TOUCHED_ORDER               -- ^ Market if Touched Order Transaction
  | MARKET_IF_TOUCHED_ORDER_REJECT        -- ^ Market if Touched Order Reject Transaction
  | TAKE_PROFIT_ORDER                     -- ^ Take Profit Order Transaction
  | TAKE_PROFIT_ORDER_REJECT              -- ^ Take Profit Order Reject Transaction
  | STOP_LOSS_ORDER                       -- ^ Stop Loss Order Transaction
  | STOP_LOSS_ORDER_REJECT                -- ^ Stop Loss Order Reject Transaction
  | TRAILING_STOP_LOSS_ORDER              -- ^ Trailing Stop Loss Order Transaction
  | TRAILING_STOP_LOSS_ORDER_REJECT       -- ^ Trailing Stop Loss Order Reject Transaction
  | ORDER_FILL                            -- ^ Order Fill Transaction
  | ORDER_CANCEL                          -- ^ Order Cancel Transaction
  | ORDER_CANCEL_REJECT                   -- ^ Order Cancel Reject Transaction
  | ORDER_CLIENT_EXTENSIONS_MODIFY        -- ^ Order Client Extensions Modify Transaction
  | ORDER_CLIENT_EXTENSIONS_MODIFY_REJECT -- ^ Order Client Extensions Modify Reject Transaction
  | TRADE_CLIENT_EXTENSIONS_MODIFY        -- ^ Trade Client Extensions Modify Transaction
  | TRADE_CLIENT_EXTENSIONS_MODIFY_REJECT -- ^ Trade Client Extensions Modify Reject Transaction
  | MARGIN_CALL_ENTER                     -- ^ Margin Call Enter Transaction
  | MARGIN_CALL_EXTEND                    -- ^ Margin Call Extend Transaction
  | MARGIN_CALL_EXIT                      -- ^ Margin Call Exit Transaction
  | DELAYED_TRADE_CLOSURE                 -- ^ Delayed Trade Closure Transaction
  | DAILY_FINANCING                       -- ^ Daily Financing Transaction
  | RESET_RESETTABLE_PL                   -- ^ Reset Resettable PL Transaction
  deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, NFData)
