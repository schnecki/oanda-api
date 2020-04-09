{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.TradeList
  ( TradeList(..)
  , prettyTradeList
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.Trade
import           Data.Oanda.Types

data TradeList = TradeList
  { trades            :: [Trade]       -- ^ The full details of the requested Account.
  , lastTransactionID :: TransactionId -- ^ The ID of the most recent Transaction created for the Account.
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)


prettyTradeList :: TradeList -> Doc
prettyTradeList (TradeList [] _)     = text "No trades"
prettyTradeList (TradeList trades _) = vcat $ map prettyTrade trades
