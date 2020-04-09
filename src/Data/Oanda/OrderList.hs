{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.OrderList
  ( OrderList(..)
  , prettyOrderList
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Time
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.Order
import           Data.Oanda.Types

data OrderList = OrderList
  { orders            :: [Order]       -- ^ The full details of the requested Account.
  , lastTransactionID :: TransactionId -- ^ The ID of the most recent Transaction created for the Account.
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)


prettyOrderList :: OrderList -> Doc
prettyOrderList (OrderList [] _)   = text "No orders"
prettyOrderList (OrderList ords _) = vcat $ map prettyOrder ords
