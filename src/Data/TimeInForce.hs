{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module Data.TimeInForce
    ( TimeInForce (..)

    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics


data TimeInForce
  = GTC -- ^ The Order is “Good unTil Cancelled”
  | GTD -- ^ The Order is “Good unTil Date” and will be cancelled at the provided time
  | GFD -- ^ The Order is “Good For Day” and will be cancelled at 5pm New York time
  | FOK -- ^ The Order must be immediately “Filled Or Killed”
  | IOC -- ^ The Order must be “Immediatedly paritally filled Or Cancelled”
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Generic, NFData)
