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
import           Data.Oanda.PriceValue
import           Data.Oanda.Types


data PositionSide = PositionSide
  { units                   :: Double -- ^ Number of units in the position (negative value indicates short position, positive indicates long position).
  , averagePrice            :: PriceValue -- ^ Volume-weighted average of the underlying Trade open prices for the Position.
  , tradeIDs                :: [TradeId] -- ^ List of the open Trade IDs which contribute to the open Position.
  , pl                      :: AccountUnits -- ^ Profit/loss realized by the PositionSide over the lifetime of the Account.
  , unrealizedPL            :: AccountUnits -- ^ The unrealized profit/loss of all open Trades that contribute to this PositionSide.
  , resettablePL            :: AccountUnits -- ^ Profit/loss realized by the PositionSide since the Account’s resettablePL was last reset by the client.
  , financing               :: AccountUnits -- ^ The total amount of financing paid/collected for this PositionSide over the lifetime of the Account.
  , guaranteedExecutionFees :: AccountUnits -- ^ The total amount of fees charged over the lifetime of the Account for the execution of guaranteed Stop Loss Orders attached to Trades for this PositionSide.
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)


prettyPositionSide :: PositionSide -> Doc
prettyPositionSide side =
  colName "units"                   $$ nest nestCols (text $ show $ units side) $+$               -- Double
  colName "averagePrice"            $$ nest nestCols (text $ show $ averagePrice side) $+$        -- PriceValue
  colName "tradeIDs"                $$ nest nestCols (text $ show $ tradeIDs side) $+$            -- [TradeId]
  colName "pl"                      $$ nest nestCols (text $ show $ pl side) $+$                  -- AccountUnits
  colName "unrealizedPL"            $$ nest nestCols (text $ show $ unrealizedPL side) $+$        -- AccountUnits
  colName "resettablePL"            $$ nest nestCols (text $ show $ resettablePL side) $+$        -- AccountUnits
  colName "financing"               $$ nest nestCols (text $ show $ financing side) $+$           -- AccountUnits
  colName "guaranteedExecutionFees" $$ nest nestCols (text $ show $ guaranteedExecutionFees side) -- AccountUnits
