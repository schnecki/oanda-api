{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.TradeSummary
    ( TradeSummary (..)
    , prettyTadeSummary
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.AccountUnits
import           Data.Oanda.ClientExtensions
import           Data.Oanda.DateTime
import           Data.Oanda.DecimalNumber
import           Data.Oanda.PriceValue
import           Data.Oanda.TradeState
import           Data.Oanda.Types

data TradeSummary = TradeSummary
  { id                      :: TradeId                -- ^ The Trade’s identifier, unique within the Trade’s Account.
  , instrument              :: InstrumentName         -- ^ The Trade’s Instrument.
  , price                   :: PriceValue             -- ^ The execution price of the Trade.
  , openTime                :: DateTime               -- ^ The date/time when the Trade was opened.
  , state                   :: TradeState             -- ^ The current state of the Trade.
  , initialUnits            :: DecimalNumber          -- ^ The initial size of the Trade. Negative values indicate a short Trade, and positive values indicate a long Trade.
  , initialMarginRequired   :: AccountUnits           -- ^ The margin required at the time the Trade was created. Note, this is the ‘pure’ margin required, it is not the ‘effective’ margin used that factors in the trade risk if a GSLO is attached to the trade.
  , currentUnits            :: DecimalNumber          -- ^ The number of units currently open for the Trade. This value is reduced to 0.0 as the Trade is closed.
  , realizedPL              :: AccountUnits           -- ^ The total profit/loss realized on the closed portion of the Trade.
  , unrealizedPL            :: AccountUnits           -- ^ The unrealized profit/loss on the open portion of the Trade.
  , marginUsed              :: AccountUnits           -- ^ Margin currently used by the Trade.
  , averageClosePrice       :: Maybe PriceValue       -- ^ The average closing price of the Trade. Only present if the Trade has been closed or reduced at least once.
  , closingTransactionIDs   :: Maybe [TransactionId]  -- ^ The IDs of the Transactions that have closed portions of this Trade.
  , financing               :: AccountUnits           -- ^ The financing paid/collected for this Trade.
  , closeTime               :: Maybe DateTime         -- ^ The date/time when the Trade was fully closed. Only provided for Trades whose state is CLOSED.
  , clientExtensions        :: Maybe ClientExtensions -- ^ The client extensions of the Trade.
  , takeProfitOrderID       :: Maybe OrderId          -- ^ ID of the Trade’s Take Profit Order, only provided if such an Order exists.
  , stopLossOrderID         :: Maybe OrderId          -- ^ ID of the Trade’s Stop Loss Order, only provided if such an Order exists.
  , trailingStopLossOrderID :: Maybe OrderId          -- ^ ID of the Trade’s Trailing Stop Loss Order only provided if such an Order exists.
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)

prettyTadeSummary :: TradeSummary -> Doc
prettyTadeSummary trade = undefined
  colName "id"                                                                  $$ nest nestCols (text $ show $ Data.Oanda.TradeSummary.id trade) $+$
  colName "instrument"                                                          $$ nest nestCols (text $ show $ instrument trade) $+$
  colName "price"                                                               $$ nest nestCols (text $ show $ price trade) $+$
  colName "openTime"                                                            $$ nest nestCols (text $ show $ openTime trade) $+$
  colName "state"                                                               $$ nest nestCols (text $ show $ state trade) $+$
  colName "initialUnits"                                                        $$ nest nestCols (text $ show $ initialUnits trade) $+$
  colName "initialMarginRequired"                                               $$ nest nestCols (text $ show $ initialMarginRequired trade) $+$
  colName "currentUnits"                                                        $$ nest nestCols (text $ show $ currentUnits trade) $+$
  colName "realizedPL"                                                          $$ nest nestCols (text $ show $ realizedPL trade) $+$
  colName "unrealizedPL"                                                        $$ nest nestCols (text $ show $ unrealizedPL trade) $+$
  colName "marginUsed"                                                          $$ nest nestCols (text $ show $ marginUsed trade) $+$
  mVal (averageClosePrice trade) (\v -> colName "averageClosePrice"             $$ nest nestCols (text $ show v)) $+$
  mVal (closingTransactionIDs trade) (\v -> colName "closingTransactionIDs"     $$ nest nestCols (text $ show v)) $+$
  colName "financing"                                                           $$ nest nestCols (text $ show $ financing trade) $+$
  mVal (closeTime trade) (\v -> colName "closeTime"                             $$ nest nestCols (text $ show v)) $+$
  mVal (clientExtensions trade) (\v -> colName "clientExtensions"               $$ nest nestCols (text $ show v)) $+$
  mVal (takeProfitOrderID trade) (\v -> colName "takeProfitOrderID"             $$ nest nestCols (text $ show v)) $+$
  mVal (stopLossOrderID trade) (\v -> colName "stopLossOrderID"                 $$ nest nestCols (text $ show v)) $+$
  mVal (trailingStopLossOrderID trade) (\v -> colName "trailingStopLossOrderID" $$ nest nestCols (text $ show v))
