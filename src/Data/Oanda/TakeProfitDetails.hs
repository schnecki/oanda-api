{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module Data.Oanda.TakeProfitDetails
    ( TakeProfitDetails (..)
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics

import           Data.Oanda.ClientExtensions
import           Data.Oanda.DateTime
import           Data.Oanda.PriceValue
import           Data.Oanda.TimeInForce

data TakeProfitDetails = TakeProfitDetails
  { price            :: PriceValue       -- ^ The price that the Take Profit Order will be triggered at. Only one of the price and distance fields may be specified.
  , timeInForce      :: TimeInForce      -- ^ The time in force for the created Take Profit Order. This may only be GTC, GTD or GFD.  [default=GTC]
  , gtdTime          :: DateTime         -- ^ The date when the Take Profit Order will be cancelled on if timeInForce is GTD.
  , clientExtensions :: ClientExtensions -- ^ The Client Extensions to add to the Take Profit Order when created.
  } deriving (Show, Eq, Ord, FromJSON, ToJSON, Generic, NFData)
