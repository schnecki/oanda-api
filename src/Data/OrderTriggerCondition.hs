{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module Data.OrderTriggerCondition
    ( OrderTriggerCondition (..)

    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics


data OrderTriggerCondition
  = DEFAULT -- ^ Trigger an Order the “natural” way: compare its price to the ask for long Orders and bid for short Orders.
  | INVERSE -- ^ Trigger an Order the opposite of the “natural” way: compare its price the bid for long Orders and ask for short Orders.
  | BID -- ^ Trigger an Order by comparing its price to the bid regardless of whether it is long or short.
  | ASK -- ^ Trigger an Order by comparing its price to the ask regardless of whether it is long or short.
  | MID -- ^ Trigger an Order by comparing its price to the midpoint regardless of whether it is long or short.
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Generic, NFData)
