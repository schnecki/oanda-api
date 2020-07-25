{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.WeeklyAlignment
    ( WeeklyAlignment (..)
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics


data WeeklyAlignment
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, NFData)
