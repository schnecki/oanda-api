{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.Order
    ( Order (..)
    , prettyOrder
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Text
import qualified Data.Text                   as T
import           Data.Time
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.ClientExtensions
import           Data.Oanda.DateTime
import           Data.Oanda.OrderState
import           Data.Oanda.Types

data Order = Order
  { id               :: OrderId          -- ^ The Order’s identifier, unique within the Order’s Account.
  , createTime       :: DateTime         -- ^ The time when the Order was created.
  , state            :: OrderState       -- ^ The current state of the Order.
  , clientExtensions :: ClientExtensions -- ^ The client extensions of the Order. Do not set, modify, or delete
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)

prettyOrder :: Order -> Doc
prettyOrder order =
  colName "id"               $$ nest nestCols (text $ T.unpack $ Data.Oanda.Order.id order) $+$
  colName "createTime"       $$ nest nestCols (text $ show $ createTime order) $+$
  colName "state"            $$ nest nestCols (text $ show $ state order) $+$
  colName "clientExtensions" $$ nest nestCols (prettyClientExtensions $ clientExtensions order)


