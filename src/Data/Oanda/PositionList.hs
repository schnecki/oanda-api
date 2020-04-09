{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.PositionList
  ( PositionList(..)
  , prettyPositionList
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.Position
import           Data.Oanda.Types

data PositionList = PositionList
  { positions         :: [Position]    -- ^ The full details of the requested Account.
  , lastTransactionID :: TransactionId -- ^ The ID of the most recent Transaction created for the Account.
  } deriving (Show, Eq, Ord, FromJSON, Generic, NFData)


prettyPositionList :: PositionList -> Doc
prettyPositionList (PositionList [] _)     = text "No positions"
prettyPositionList (PositionList trades _) = vcat $ map prettyPosition trades
