{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


-- ^ A TradeReduce object represents a Trade for an instrument that was reduced (either partially or fully) in an
-- Account. It is found embedded in Transactions that affect the position of an instrument in the account, specifically
-- the OrderFill Transaction.
module Data.Oanda.TradeReduce
  ( TradeReduce(..)
  , prettyTradeReduce
  ) where

import           Control.DeepSeq
import           Data.Aeson
import qualified Data.Text                as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.AccountUnits
import           Data.Oanda.DecimalNumber
import           Data.Oanda.PriceValue
import           Data.Oanda.Types


data TradeReduce = TradeReduce
  { tradeID                :: TradeId       -- ^ The ID of the Trade that was reduced or closed
  , units                  :: DecimalNumber -- ^ The number of units that the Trade was reduced by
  , price                  :: PriceValue    -- ^ The average price that the units were closed at. This price may be     clamped for guaranteed Stop Loss Orders.
  , realizedPL             :: AccountUnits  -- ^ The PL realized when reducing the Trade
  , financing              :: AccountUnits  -- ^ The financing paid/collected when reducing the Trade
  , guaranteedExecutionFee :: AccountUnits  -- ^ This is the fee that is charged for closing the Trade if it has a     guaranteed Stop Loss Order attached to it.
  , halfSpreadCost         :: AccountUnits  -- ^ The half spread cost for the trade reduce/close. This can be a positive     or negative value and is represented in the home currency of the Account.
  } deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, NFData)


prettyTradeReduce :: TradeReduce -> Doc
prettyTradeReduce red =
  colName "tradeID"                $$ nest nestCols (text $ T.unpack $ tradeID red) $+$            -- TradeId
  colName "units"                  $$ nest nestCols (text $ show $ units red) $+$                  -- DecimalNumber
  colName "price"                  $$ nest nestCols (text $ show $ price red) $+$                  -- PriceValue
  colName "realizedPL"             $$ nest nestCols (text $ show $ realizedPL red) $+$             -- AccountUnits
  colName "financing"              $$ nest nestCols (text $ show $ financing red) $+$              -- AccountUnits
  colName "guaranteedExecutionFee" $$ nest nestCols (text $ show $ guaranteedExecutionFee red) $+$ -- AccountUnits
  colName "halfSpreadCost"         $$ nest nestCols (text $ show $ halfSpreadCost red)             -- AccountUnits
