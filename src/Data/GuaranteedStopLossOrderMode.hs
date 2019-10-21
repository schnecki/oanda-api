{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.GuaranteedStopLossOrderMode
    ( GuaranteedStopLossOrderMode (..)

    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Text
import           GHC.Generics


data GuaranteedStopLossOrderMode
  = DISABLED
  | ALLOWED
  | REQUIRED
  deriving (Show, Eq, Ord, FromJSON, Generic, NFData)
