{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.InstrumentCommission
    ( InstrumentCommission (..)

    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics

import           Data.DecimalNumber

data InstrumentCommission = InstrumentCommission
  { commission        :: DecimalNumber -- ^ The commission amount (in the Account’s home currency) charged per     unitsTraded of the instrument
  , unitsTraded       :: DecimalNumber -- ^ The number of units traded that the commission amount is based on.
  , minimumCommission :: DecimalNumber -- ^ The minimum commission amount (in the Account’s home currency) that is     charged when an Order is filled for this instrument.
  } deriving (Generic, Show, Eq, Ord, FromJSON, NFData)
