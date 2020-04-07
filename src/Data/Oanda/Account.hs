{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}


module Data.Oanda.Account
  ( Account(..)
  , prettyAccountSummary
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           GHC.Generics
import           Prelude                                hiding ((<>))
import           Text.PrettyPrint

import           Data.Oanda.AccountUnits
import           Data.Oanda.Currency
import           Data.Oanda.DateTime
import           Data.Oanda.GuaranteedStopLossOrderMode
import           Data.Oanda.Order
import           Data.Oanda.Position                    hiding (commission, financing,
                                                         guaranteedExecutionFees,
                                                         marginUsed, pl, resettablePL,
                                                         unrealizedPL)
import           Data.Oanda.TradeSummary                hiding (financing, marginUsed,
                                                         unrealizedPL)
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


-- | Prints account without open trades, positions and prending orders
prettyAccountSummary :: Account -> Doc
prettyAccountSummary acc =
  colName "Account ID"                                                   $$ nest nestCols (text $ T.unpack $ accountId acc) $+$
  mVal (alias acc) (\a -> colName "Alias"                                $$ nest nestCols (text $ T.unpack a)) $+$
  colName "Currency"                                                     $$ nest nestCols (text $ show $ currency acc) $+$
  colName "Balance"                                                      $$ nest nestCols (prettyAccountUnits $ balance acc) $+$
  -- colName "createdByUserID"                                           $$ nest nestCols (text $ show $ createdByUserID acc) $+$
  -- colName "createdTime"                                               $$ nest nestCols (text $ show $ createdTime acc) $+$
  colName "Guaranteed Stop Loss Order Mode"                              $$ nest nestCols (text $ show $ guaranteedStopLossOrderMode acc) $+$
  colName "Total Profit/Loss Realised"                                   $$ nest nestCols (prettyAccountUnits $ pl acc) $+$
  colName "Total Profit/Loss Realised Since Last Reset"                  $$ nest nestCols (prettyAccountUnits $ resettablePL acc) $+$
  colName "Last Total Profit/Loss Reset Time"                            $$ nest nestCols (text $ show $ resettablePLTime acc) $+$
  colName "Financing Paid/Collected"                                     $$ nest nestCols (prettyAccountUnits $ financing acc) $+$
  colName "Commission Paid"                                              $$ nest nestCols (prettyAccountUnits $ commission acc) $+$
  colName "Fees for Guaranteed Stop Loss Orders"                         $$ nest nestCols (prettyAccountUnits $ guaranteedExecutionFees acc) $+$
  colName "Margin Rate"                                                  $$ nest nestCols (mDefVal "None set" (marginRate acc) (text . T.unpack)) $+$
  colName "Margin Call Enter Time"                                       $$ nest nestCols (text $ show $ marginCallEnterTime acc) $+$
  colName "Margin Call Extension Count"                                  $$ nest nestCols (text $ show $ marginCallExtensionCount acc) $+$
  colName "Last Margin Call Extension Time"                              $$ nest nestCols (text $ show $ lastMarginCallExtensionTime acc) $+$
  colName "Trades Currently Open"                                        $$ nest nestCols (text $ show $ openTradeCount acc) $+$
  colName "Positions Currently Open"                                     $$ nest nestCols (text $ show $ openPositionCount acc) $+$
  colName "Prending Order Count"                                         $$ nest nestCols (text $ show $ pendingOrderCount acc) $+$
  colName "Hedging Enabled"                                              $$ nest nestCols (text $ show $ hedgingEnabled acc) $+$
  colName "Unrealised Profit/Loss for Open Trades"                       $$ nest nestCols (prettyAccountUnits $ unrealizedPL acc) $+$
  colName "Net Asset Value (Balance + Unrealised Profit/Loss)"           $$ nest nestCols (prettyAccountUnits $ nav acc) $+$
  colName "Margin Used"                                                  $$ nest nestCols (prettyAccountUnits $ marginUsed acc) $+$
  colName "Margin Available"                                             $$ nest nestCols (prettyAccountUnits $ marginAvailable acc) $+$
  colName "Open Position Value in Home Currency"                         $$ nest nestCols (prettyAccountUnits $ positionValue acc) $+$
  colName "Margin Closeout Unrealized Profit/Loss"                       $$ nest nestCols (prettyAccountUnits $ marginCloseoutUnrealizedPL acc) $+$
  colName "Margin Closeout NAV"                                          $$ nest nestCols (prettyAccountUnits $ marginCloseoutNAV acc) $+$
  colName "Margin Closeout Margin Used"                                  $$ nest nestCols (prettyAccountUnits $ marginCloseoutMarginUsed acc) $+$
  colName "Margin Closeout Percent (if >=1.0 then margin closeout sit.)" $$ nest nestCols (text $ T.unpack $ marginCloseoutPercent acc) $+$
  colName "Margin Closeout Position Value"                               $$ nest nestCols (text $ T.unpack $ marginCloseoutPositionValue acc) $+$
  colName "Withdrawal Limit"                                             $$ nest nestCols (prettyAccountUnits $ withdrawalLimit acc) $+$
  colName "Margin Call Margin Used"                                      $$ nest nestCols (prettyAccountUnits $ marginCallMarginUsed acc) $+$
  colName "Margin Call Percent (if >=1.0 then margin call situation)"    $$ nest nestCols (text $ T.unpack $ marginCallPercent acc) $+$
  colName "Last Transaction ID"                                          $$ nest nestCols (text $ T.unpack $ lastTransactionID acc)
  -- colName "Open Trades"                                                  $$ nest nestCols (text $ show $ trades acc) $+$
  -- colName "Positions"                                                    $$ nest nestCols (text $ show $ positions acc) $+$
  -- colName "Pending Orders"                                               $$ nest nestCols (text $ show $ orders acc)
  where colName n = text n <> colon
        mVal Nothing _     = mempty
        mVal (Just v) line = line v
        mDefVal def Nothing _   = def
        mDefVal _ (Just v) line = line v

