{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Position
    ( Position (..)

    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Text
import           Data.Time
import           GHC.Generics

import           Types

import           Data.AccountUnits
import           Data.PositionSide

data Position = Position
  { instrument              :: InstrumentName -- ^ The Position’s Instrument.
  , pl                      :: AccountUnits -- ^ Profit/loss realized by the Position over the lifetime of the Account.
  , unrealizedPL            :: AccountUnits -- ^ The unrealized profit/loss of all open Trades that contribute to this Position.
  , marginUsed              :: AccountUnits -- ^ Margin currently used by the Position.
  , resettablePL            :: AccountUnits -- ^ Profit/loss realized by the Position since the Account’s resettablePL was last reset by the client.
  , financing               :: AccountUnits -- ^ The total amount of financing paid/collected for this instrument over the lifetime of the Account.
  , commission              :: AccountUnits -- ^ The total amount of commission paid for this instrument over the lifetime of the Account.
  , guaranteedExecutionFees :: AccountUnits -- ^ The total amount of fees charged over the lifetime of the Account for the execution of guaranteed Stop Loss Orders for this instrument.
  , long                    :: PositionSide -- ^ The details of the long side of the Position.
  , short                   :: PositionSide -- ^ The details of the short side of the Position.
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)
