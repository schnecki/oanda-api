{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.ClientExtensions
    ( ClientExtensions

    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics

import           Types

data ClientExtensions = ClientExtensions
  { id      :: ClientId         -- ^ The Client ID of the Order/Trade
  , tag     :: ClientTag        -- ^ A tag associated with the Order/Trade
  , comment :: ClientComment    -- ^ A comment associated with the Order/Trade
  } deriving (Show, Eq, Ord, FromJSON, ToJSON, Generic, NFData)
