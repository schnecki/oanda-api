{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.Position
    ( Position (..)
    , prettyPosition
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Text
import qualified Data.Text               as T
import           Data.Time
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.Types

import           Data.Oanda.AccountUnits
import           Data.Oanda.PositionSide

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


prettyPosition :: Position -> Doc
prettyPosition pos =
  colName "instrument"              $$ nest nestCols (text $ T.unpack $ instrument pos) $+$                              -- InstrumentName
  colName "pl"                      $$ nest nestCols (text $ show $ Data.Oanda.Position.pl pos) $+$                      -- AccountUnits
  colName "unrealizedPL"            $$ nest nestCols (text $ show $ Data.Oanda.Position.unrealizedPL pos) $+$            -- AccountUnits
  colName "marginUsed"              $$ nest nestCols (text $ show $ marginUsed pos) $+$                                  -- AccountUnits
  colName "resettablePL"            $$ nest nestCols (text $ show $ Data.Oanda.Position.resettablePL pos) $+$            -- AccountUnits
  colName "financing"               $$ nest nestCols (text $ show $ Data.Oanda.Position.financing pos) $+$               -- AccountUnits
  colName "commission"              $$ nest nestCols (text $ show $ commission pos) $+$                                  -- AccountUnits
  colName "guaranteedExecutionFees" $$ nest nestCols (text $ show $ Data.Oanda.Position.guaranteedExecutionFees pos) $+$ -- AccountUnits
  colName "long"                    $$ nest nestCols (prettyPositionSide $ long pos) $+$                                 -- PositionSide
  colName "short"                   $$ nest nestCols (prettyPositionSide $ short pos)                                    -- PositionSide
