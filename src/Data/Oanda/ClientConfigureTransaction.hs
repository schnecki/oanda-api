{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Oanda.ClientConfigureTransaction
    ( ClientConfigureTransaction (..)
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text
import           GHC.Generics

import           Data.Oanda.DateTime
import           Data.Oanda.DecimalNumber
import           Data.Oanda.TransactionType
import           Types

data ClientConfigureTransaction = ClientConfigureTransaction
  { id              :: TransactionId   -- ^ The Transaction’s Identifier.
  , time            :: DateTime        -- ^ The date/time when the Transaction was created.
  , userID          :: Int             -- ^ The ID of the user that initiated the creation of the Transaction.
  , accountID       :: AccountId       -- ^ The ID of the Account the Transaction was created for.
  , batchID         :: TransactionId   -- ^ The ID of the “batch” that the Transaction belongs to. Transactions in the
                                       -- same batch are applied to the Account simultaneously.
  , requestID       :: RequestId       -- ^ The Request ID of the request which generated the transaction.
  , transactionType :: TransactionType -- ^ The Type of the Transaction. Always set to “CLIENT_CONFIGURE” in a
                                       -- ClientConfigureTransaction.
  , alias           :: Text            -- ^ The client-provided alias for the Account.
  , marginRate      :: DecimalNumber   -- ^ The margin rate override for the Account.
  } deriving (Show, Eq, Ord, Generic, NFData)

-- Derive from JSON by modifying the NAV-nav field.
$(deriveFromJSON
    defaultOptions
      { fieldLabelModifier =
          let f "transactionType" = "type"
              f x                 = x
           in f
      }
    ''ClientConfigureTransaction)
