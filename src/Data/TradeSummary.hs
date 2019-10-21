{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.TradeSummary
    ( TradeSummary (..)

    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Text
import           Data.Time
import           GHC.Generics

import           Data.AccountUnits
import           Data.ClientExtensions
import           Data.DateTime
import           Data.PriceValue
import           Data.TradeState
import           Types

data TradeSummary = TradeSummary
  { id                      :: TradeId          -- ^ The Trade’s identifier, unique within the Trade’s Account.
  , instrument              :: InstrumentName   -- ^ The Trade’s Instrument.
  , price                   :: PriceValue -- ^ The execution price of the Trade.
  , openTime                :: DateTime -- ^ The date/time when the Trade was opened.
  , state                   :: TradeState -- ^ The current state of the Trade.
  , initialUnits            :: Double -- ^ The initial size of the Trade. Negative values indicate a short Trade, and positive values indicate a long Trade.
  , initialMarginRequired   :: AccountUnits -- ^ The margin required at the time the Trade was created. Note, this is the ‘pure’ margin required, it is not the ‘effective’ margin used that factors in the trade risk if a GSLO is attached to the trade.
  , currentUnits            :: Double -- ^ The number of units currently open for the Trade. This value is reduced to 0.0 as the Trade is closed.
  , realizedPL              :: AccountUnits -- ^ The total profit/loss realized on the closed portion of the Trade.
  , unrealizedPL            :: AccountUnits -- ^ The unrealized profit/loss on the open portion of the Trade.
  , marginUsed              :: AccountUnits -- ^ Margin currently used by the Trade.
  , averageClosePrice       :: Maybe PriceValue -- ^ The average closing price of the Trade. Only present if the Trade has been closed or reduced at least once.
  , closingTransactionIDs   :: [TransactionId] -- ^ The IDs of the Transactions that have closed portions of this Trade.
  , financing               :: AccountUnits    -- ^ The financing paid/collected for this Trade.
  , closeTime               :: Maybe DateTime -- ^ The date/time when the Trade was fully closed. Only provided for Trades whose state is CLOSED.
  , clientExtensions        :: ClientExtensions -- ^ The client extensions of the Trade.
  , takeProfitOrderID       :: Maybe OrderId -- ^ ID of the Trade’s Take Profit Order, only provided if such an Order exists.
  , stopLossOrderID         :: Maybe OrderId -- ^ ID of the Trade’s Stop Loss Order, only provided if such an Order exists.
  , trailingStopLossOrderID :: Maybe OrderId -- ^ ID of the Trade’s Trailing Stop Loss Order only provided if such an Order exists.
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)
