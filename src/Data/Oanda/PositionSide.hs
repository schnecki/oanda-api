{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.PositionSide
    ( PositionSide (..)
    , prettyPositionSide
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.AccountUnits
import           Data.Oanda.DecimalNumber
import           Data.Oanda.PriceValue
import           Data.Oanda.Types


data PositionSide = PositionSide
  { units                   :: DecimalNumber    -- ^ Number of units in the position (negative value indicates short position, positive indicates long position).
  , averagePrice            :: Maybe PriceValue -- ^ Volume-weighted average of the underlying Trade open prices for the Position.
  , tradeIDs                :: Maybe [TradeId]  -- ^ List of the open Trade IDs which contribute to the open Position.
  , pl                      :: AccountUnits     -- ^ Profit/loss realized by the PositionSide over the lifetime of the Account.
  , unrealizedPL            :: AccountUnits     -- ^ The unrealized profit/loss of all open Trades that contribute to this PositionSide.
  , resettablePL            :: AccountUnits     -- ^ Profit/loss realized by the PositionSide since the Account’s resettablePL was last reset by the client.
  , financing               :: AccountUnits     -- ^ The total amount of financing paid/collected for this PositionSide over the lifetime of the Account.
  , guaranteedExecutionFees :: AccountUnits     -- ^ The total amount of fees charged over the lifetime of the Account for the execution of guaranteed Stop Loss Orders attached to Trades for this PositionSide.
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)


prettyPositionSide :: PositionSide -> Doc
prettyPositionSide side =
  colName "units"                                        $$ nest nestCols (prettyDecimalNumber $ units side) $+$              -- DecimalNumber
  mVal (averagePrice side) (\v -> colName "averagePrice" $$ nest nestCols (prettyPriceValue v)) $+$                           -- PriceValue
  mVal (tradeIDs side) (\v -> colName "tradeIDs"         $$ nest nestCols (text $ show v)) $+$                                -- [TradeId]
  colName "pl"                                           $$ nest nestCols (prettyAccountUnits $ pl side) $+$                  -- AccountUnits
  colName "unrealizedPL"                                 $$ nest nestCols (prettyAccountUnits $ unrealizedPL side) $+$        -- AccountUnits
  colName "resettablePL"                                 $$ nest nestCols (prettyAccountUnits $ resettablePL side) $+$        -- AccountUnits
  colName "financing"                                    $$ nest nestCols (prettyAccountUnits $ financing side) $+$           -- AccountUnits
  colName "guaranteedExecutionFees"                      $$ nest nestCols (prettyAccountUnits $ guaranteedExecutionFees side) -- AccountUnits
