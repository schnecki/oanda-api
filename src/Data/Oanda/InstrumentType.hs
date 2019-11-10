{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.InstrumentType
  ( InstrumentType(..)
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics


data InstrumentType
  = CURRENCY -- ^ Currency
  | CFD      -- ^ Contract For Difference
  | METAL    -- ^ Metal
  deriving (Generic, Show, Eq, Ord, FromJSON, NFData)
