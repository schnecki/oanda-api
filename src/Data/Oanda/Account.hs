{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}


module Data.Oanda.Account
  ( Account(..)
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text                              (Text)
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.AccountUnits
import           Data.Oanda.Currency
import           Data.Oanda.DateTime
import           Data.Oanda.GuaranteedStopLossOrderMode
import           Data.Oanda.Order
import           Data.Oanda.Position
import           Data.Oanda.TradeSummary
import           Data.Oanda.Types


data Account = Account
 {
    accountId                     :: AccountId                   -- ^ The Account’s identifier
    , alias                       :: Maybe Text                  -- ^ Client-assigned alias for the Account. Only provided if the Account has an alias set
    , currency                    :: Currency                    -- ^ The home currency of the Account
    , balance                     :: AccountUnits                -- ^ The current balance of the Account.
    , createdByUserID             :: Int                         -- ^ ID of the user that created the Account.
    , createdTime                 :: DateTime                    -- ^ The date/time when the Account was created.
    , guaranteedStopLossOrderMode :: GuaranteedStopLossOrderMode -- ^ The current guaranteed Stop Loss Order mode of the Account.
    , pl                          :: AccountUnits                -- ^ The total profit/loss realized over the lifetime of the Account.
    , resettablePL                :: AccountUnits                -- ^ The total realized profit/loss for the Account since it was last reset by the client.
    , resettablePLTime            :: DateTime                    -- ^ The date/time that the Account’s resettablePL was last reset.
    , financing                   :: AccountUnits                -- ^ The total amount of financing paid/collected over the lifetime of the Account.
    , commission                  :: AccountUnits                -- ^ The total amount of commission paid over the lifetime of the Account.
    , guaranteedExecutionFees     :: AccountUnits                -- ^ The total amount of fees charged over the lifetime of the Account for the execution of guaranteed Stop Loss Orders.
    , marginRate                  :: Maybe MarginRate            -- ^ Client-provided margin rate override for the Account. The effective margin rate of the Account is the lesser of this value
                                                                 -- and the OANDA margin rate for the Account’s division. This value is only provided if a margin rate override exists for the Account.
    , marginCallEnterTime         :: Maybe DateTime              -- ^ The date/time when the Account entered a margin call state. Only provided if the Account is in a margin call.
    , marginCallExtensionCount    :: Maybe Int                   -- ^ The number of times that the Account’s current margin call was extended.
    , lastMarginCallExtensionTime :: Maybe DateTime              -- ^ The date/time of the Account’s last margin call extension.
    , openTradeCount              :: Int                         -- ^ The number of Trades currently open in the Account.
    , openPositionCount           :: Int                         -- ^ The number of Positions currently open in the Account.
    , pendingOrderCount           :: Int                         -- ^ The number of Orders currently pending in the Account.
    , hedgingEnabled              :: Bool                        -- ^ Flag indicating that the Account has hedging enabled.
    , unrealizedPL                :: AccountUnits                -- ^ The total unrealized profit/loss for all Trades currently open in the Account.
    , nav                         :: AccountUnits                -- ^ The net asset value of the Account. Equal to Account balance + unrealizedPL.
    , marginUsed                  :: AccountUnits                -- ^ Margin currently used for the Account.
    , marginAvailable             :: AccountUnits                -- ^ Margin available for Account currency.
    , positionValue               :: AccountUnits                -- ^ The value of the Account’s open positions represented in the Account’s home currency.
    , marginCloseoutUnrealizedPL  :: AccountUnits                -- ^ The Account’s margin closeout unrealized PL.
    , marginCloseoutNAV           :: AccountUnits                -- ^ The Account’s margin closeout NAV.
    , marginCloseoutMarginUsed    :: AccountUnits                -- ^ The Account’s margin closeout margin used.
    , marginCloseoutPercent       :: Percentage                  -- ^ The Account’s margin closeout percentage. When this value is 1.0 or above the Account is in a margin closeout situation.
    , marginCloseoutPositionValue :: PositionValue               -- ^ The value of the Account’s open positions as used for margin closeout calculations represented in the Account’s home currency.
    , withdrawalLimit             :: AccountUnits                -- ^ The current WithdrawalLimit for the account which will be zero or a positive value indicating how much can be withdrawn from the account.
    , marginCallMarginUsed        :: AccountUnits                -- ^ The Account’s margin call margin used.
    , marginCallPercent           :: Percentage                  -- ^ The Account’s margin call percentage. When this value is 1.0 or above the Account is in a margin call situation.
    , lastTransactionID           :: TransactionId               -- ^ The ID of the last Transaction created for the Account.
    , trades                      :: [TradeSummary]              -- ^ The details of the Trades currently open in the Account.
    , positions                   :: [Position]                  -- ^ The details all Account Positions.
    , orders                      :: [Order]                     -- ^ The details of the Orders currently pending in the Account.
 } deriving (Show, Eq, Ord, Generic, NFData)


-- Derive from JSON by modifying the NAV-nav field.
$(deriveFromJSON
    defaultOptions
      { fieldLabelModifier =
          let f "nav"       = "NAV"
              f "accountId" = "id"
              f x           = x
           in f
      }
    ''Account)

