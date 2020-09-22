{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.Trade
    ( Trade (..)
    , prettyTrade
    ) where

import           Control.DeepSeq
import           Data.Aeson
import qualified Data.Text                        as T
import           GHC.Generics
import           Prelude                          hiding ((<>))
import           Text.PrettyPrint

import           Data.Oanda.AccountUnits
import           Data.Oanda.ClientExtensions
import           Data.Oanda.DateTime
import           Data.Oanda.DecimalNumber
import           Data.Oanda.PriceValue
import           Data.Oanda.StopLossOrder
import           Data.Oanda.TakeProfitOrder
import           Data.Oanda.TradeState
import           Data.Oanda.TrailingStopLossOrder
import           Data.Oanda.Types

-- ^ The specification of a Trade within an Account. This includes the full representation of the Trade’s dependent
-- Orders in addition to the IDs of those Orders.
data Trade =
  Trade
    { id                    :: TradeId                     -- ^ The Trade’s identifier, unique within the Trade’s Account.
    , instrument            :: InstrumentName              -- ^ The Trade’s Instrument.
    , price                 :: PriceValue                  -- ^ The execution price of the Trade.
    , openTime              :: DateTime                    -- ^ The date/time when the Trade was opened.
    , state                 :: TradeState                  -- ^ The current state of the Trade.
    , initialUnits          :: DecimalNumber               -- ^ The initial size of the Trade. Negative values indicate a short Trade and positive values indicate a long Trade.
    , initialMarginRequired :: AccountUnits                -- ^ The margin required at the time the Trade was created. Note this is factors in the trade risk if a GSLO is attached to the trade.
    , currentUnits          :: DecimalNumber               -- ^ The number of units currently open for the Trade. This value is reduced to 0.0 as the Trade is closed.
    , realizedPL            :: AccountUnits                -- ^ The total profit/loss realized on the closed portion of the Trade.
    , unrealizedPL          :: AccountUnits                -- ^ The unrealized profit/loss on the open portion of the Trade.
    , marginUsed            :: AccountUnits                -- ^ Margin currently used by the Trade.
    , averageClosePrice     :: Maybe PriceValue            -- ^ The average closing price of the Trade. Only present if the Trade has been closed or reduced at least once.
    , closingTransactionIDs :: Maybe [TransactionId]       -- ^ The IDs of the Transactions that have closed portions of this Trade.
    , financing             :: AccountUnits                -- ^ The financing paid/collected for this Trade.
    , dividendAdjustment    :: AccountUnits                -- ^ The dividend adjustment paid for this Trade.
    , closeTime             :: Maybe DateTime              -- ^ The date/time when the Trade was fully closed. Only provided for Trades whose state is CLOSED.
    , clientExtensions      :: Maybe ClientExtensions      -- ^ The client extensions of the Trade.
    , takeProfitOrder       :: Maybe TakeProfitOrder       -- ^ Full representation of the Trade’s Take Profit Order only provided if
    , stopLossOrder         :: Maybe StopLossOrder         -- ^ Full representation of the Trade’s Stop Loss Order only provided if such an Order exists.
    , trailingStopLossOrder :: Maybe TrailingStopLossOrder -- ^ Full representation of the Trade’s Trailing Stop Loss Order only provided if such an Order exists.
    } deriving (Show, Eq, Ord, Generic, NFData, ToJSON, FromJSON)


prettyTrade :: Trade -> Doc
prettyTrade trade =
  colName "Order ID"                                                               $$ nest nestCols (text $ T.unpack $ Data.Oanda.Trade.id trade) $+$
  colName "instrument"                                                             $$ nest nestCols (text $ T.unpack $ instrument trade) $+$                               -- InstrumentName
  colName "price"                                                                  $$ nest nestCols (text $ show $ Data.Oanda.Trade.price trade) $+$                       -- PriceValue
  colName "openTime"                                                               $$ nest nestCols (text $ show $ openTime trade) $+$                                     -- DateTime
  colName "state"                                                                  $$ nest nestCols (text $ show $ Data.Oanda.Trade.state trade) $+$                       -- TradeState
  colName "initialUnits"                                                           $$ nest nestCols (prettyDecimalNumber $ initialUnits trade) $+$                         -- DecimalNumber
  colName "initialMarginRequired"                                                  $$ nest nestCols (prettyAccountUnits $ initialMarginRequired trade) $+$                 -- AccountUnits
  colName "currentUnits"                                                           $$ nest nestCols (prettyDecimalNumber $ currentUnits trade) $+$                         -- DecimalNumber
  colName "realizedPL"                                                             $$ nest nestCols (prettyAccountUnits $ realizedPL trade) $+$                            -- AccountUnits
  colName "unrealizedPL"                                                           $$ nest nestCols (prettyAccountUnits $ unrealizedPL trade) $+$                          -- AccountUnits
  colName "marginUsed"                                                             $$ nest nestCols (prettyAccountUnits $ marginUsed trade) $+$                            -- AccountUnits
  mVal (averageClosePrice trade) (\v -> colName "averageClosePrice"                $$ nest nestCols (text $ show v)) $+$                                                   -- Maybe PriceValue
  mVal (closingTransactionIDs trade) (\v -> colName "closingTransactionIDs"        $$ nest nestCols (text $ show v)) $+$                                                   -- [TransactionId]
  colName "financing"                                                              $$ nest nestCols (prettyAccountUnits $ financing trade) $+$                             -- AccountUnits
  colName "dividendAdjustment"                                                     $$ nest nestCols (prettyAccountUnits $ dividendAdjustment trade) $+$                    -- AccountUnits
  mVal (closeTime trade) (\v -> colName "closeTime"                                $$ nest nestCols (text $ show v)) $+$                                                   -- Maybe DateTime
  mVal (Data.Oanda.Trade.clientExtensions trade) (\v -> colName "clientExtensions" $$ nest nestCols (prettyClientExtensions v)) $+$                                        -- ClientExtensions
  mVal (closeTime trade) (\v -> colName "takeProfitOrder"                          $$ nest nestCols (text $ show v)) $+$                                                   -- Maybe TakeProfitOrder
  mVal (closeTime trade) (\v -> colName "stopLossOrder"                            $$ nest nestCols (text $ show v)) $+$                                                   -- Maybe StopLossOrder
  mVal (closeTime trade) (\v -> colName "trailingStopLossOrder"                    $$ nest nestCols (text $ show v))                                                       -- Maybe TrailingStopLossOrder


