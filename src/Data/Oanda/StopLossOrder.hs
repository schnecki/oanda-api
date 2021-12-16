{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Oanda.StopLossOrder
    ( StopLossOrder (..)
    , prettyStopLossOrder
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text
import qualified Data.Text                        as T
import           Data.Time
import           GHC.Generics
import           Prelude                          hiding (id)
import           Text.PrettyPrint

import           Data.Oanda.ClientExtensions
import           Data.Oanda.DateTime
import           Data.Oanda.DecimalNumber
import           Data.Oanda.OrderState
import           Data.Oanda.OrderTriggerCondition
import           Data.Oanda.OrderType
import           Data.Oanda.PriceValue
import           Data.Oanda.TimeInForce
import           Data.Oanda.Types

data StopLossOrder =
  StopLossOrder
    { id                      :: OrderId               -- ^ The Order’s identifier, unique within the Order’s Account.
    , createTime              :: DateTime              -- ^ The time when the Order was created.
    , state                   :: OrderState            -- ^ The current state of the Order.
    , clientExtensions        :: ClientExtensions      -- ^ The client extensions of the Order. Do not set, modify, or delete
                                                       -- clientExtensions if your account is associated with MT4.
    , orderType               :: OrderType             -- ^ The type of the Order. Always set to “STOP_LOSS” for Stop Loss Orders [default=STOP_LOSS].
    , tradeID                 :: TradeId               -- ^ The ID of the Trade to close when the price threshold is breached.
    , clientTradeID           :: ClientId              -- ^ The client ID of the Trade to be closed when the price threshold is breached.
    , price                   :: PriceValue            -- ^ The price threshold specified for the Stop Loss Order. The associated Trade will be
                                                       -- closed by a market price that is equal to or worse than this threshold.
    , distance                :: DecimalNumber         -- ^ Specifies the distance (in price units) from the Account’s current price to use as
                                                       -- the Stop Loss Order price. If the Trade is short the Instrument’s bid price is used,
                                                       -- and for long Trades the ask is used.
    , timeInForce             :: TimeInForce           -- ^ The time-in-force requested for the StopLoss Order. Restricted to “GTC”, “GFD” and
                                                       -- “GTD” for StopLoss Orders. [default=GTC]
    , gtdTime                 :: DateTime              -- ^ The date/time when the StopLoss Order will be cancelled if its timeInForce is “GTD”.
    , triggerCondition        :: OrderTriggerCondition -- ^ Specification of which price component should be used when
                                                       -- determining if an Order should be triggered and filled. This allows
                                                       -- Orders to be triggered based on the bid, ask, mid, default (ask for
                                                       -- buy, bid for sell) or inverse (ask for sell, bid for buy) price
                                                       -- depending on the desired behaviour. Orders are always filled using
                                                       -- their default price component. This feature is only provided through
                                                       -- the REST API. Clients who choose to specify a non-default trigger
                                                       -- condition will not see it reflected in any of OANDA’s proprietary or
                                                       -- partner trading platforms, their transaction history or their account
                                                       -- statements. OANDA platforms always assume that an Order’s trigger
                                                       -- condition is set to the default value when indicating the distance
                                                       -- from an Order’s trigger price, and will always provide the default
                                                       -- trigger condition when creating or modifying an Order. A special
                                                       -- restriction applies when creating a guaranteed Stop Loss Order. In
                                                       -- this case the TriggerCondition value must either be “DEFAULT”, or the
                                                       -- “natural” trigger side “DEFAULT” results in. So for a Stop Loss Order
                                                       -- for a long trade valid values are “DEFAULT” and “BID”, and for short
                                                       -- trades “DEFAULT” and “ASK” are valid. [default=DEFAULT]
    , fillingTransactionID    :: Maybe TransactionId   -- ^ ID of the Transaction that filled this Order (only provided when the
                                                       -- Order’s state is FILLED)
    , filledTime              :: Maybe DateTime        -- ^ Date/time when the Order was filled (only provided when the Order’s state is FILLED)
    , tradeOpenedID           :: Maybe TradeId         -- ^ Trade ID of Trade opened when the Order was filled (only provided when the Order’s
                                                       -- state is FILLED and a Trade was opened as a result of the fill)
    , tradeReducedID          :: Maybe TradeId         -- ^ Trade ID of Trade reduced when the Order was filled (only provided when the Order’s
                                                       -- state is FILLED and a Trade was reduced as a result of the fill)
    , tradeClosedIDs          :: Maybe [TradeId]       -- ^ Trade IDs of Trades closed when the Order was filled (only provided when the
                                                       -- Order’s state is FILLED and one or more Trades were closed as a result of the fill)
    , cancellingTransactionID :: Maybe TransactionId   -- ^ ID of the Transaction that cancelled the Order (only provided when
                                                       -- the Order’s state is CANCELLED)
    , cancelledTime           :: Maybe DateTime        -- ^ Date/time when the Order was cancelled (only provided when the state of the Order
                                                       -- is CANCELLED)
    , replacesOrderId         :: Maybe OrderId         -- ^ The ID of the Order that was replaced by this Order (only provided if this Order
                                                       -- was created as part of a cancel/replace).
    , replacedByOrderId       :: Maybe OrderId         -- ^ The ID of the Order that replaced this Order (only provided if this Order was
                                                       -- cancelled as part of a cancel/replace).
    }
  deriving (Show, Eq, Ord, Generic, NFData)

-- Derive from JSON by modifying the NAV-nav field.
$(deriveToJSON
    defaultOptions
      { omitNothingFields = True,
        fieldLabelModifier =
          let f "orderType" = "type"
              f x           = x
           in f
      }
    ''StopLossOrder)


-- Derive from JSON by modifying the NAV-nav field.
$(deriveFromJSON
    defaultOptions
      { omitNothingFields = True,
        fieldLabelModifier =
          let f "orderType" = "type"
              f x           = x
           in f
      }
    ''StopLossOrder)


    --
    --  The premium that will be charged if the Stop Loss Order is guaranteed and
    --  the Order is filled at the guaranteed price. It is in price units and is
    --  charged for each unit of the Trade.
    --
    --
    --  Deprecated: Will be removed in a future API update.
    --
    -- guaranteedExecutionPremium : (DecimalNumber, deprecated),

    --
    --  Flag indicating that the Stop Loss Order is guaranteed. The default value depends on the
    --  GuaranteedStopLossOrderMode of the account, if it is REQUIRED, the default will be true, for DISABLED or ENABLED
    --  the default is false.
    --
    --
    --  Deprecated: Will be removed in a future API update.
    --
    -- guaranteed : (boolean, deprecated),

prettyStopLossOrder :: StopLossOrder -> Doc
prettyStopLossOrder order =
    colName "Order ID" $$ nest nestCols (text $ T.unpack $ Data.Oanda.StopLossOrder.id order) $+$
    colName "createTime"              $$ nest nestCols (text $ show $ createTime order) $+$
    colName "state"                   $$ nest nestCols (text $ show $ state order) $+$
    colName "clientExtensions"        $$ nest nestCols (prettyClientExtensions $ clientExtensions order) $+$
    colName "orderType"               $$ nest nestCols (text $ show $ orderType order) $+$
    colName "tradeID"                 $$ nest nestCols (text $ T.unpack $ tradeID order) $+$
    colName "clientTradeID"           $$ nest nestCols (text $ T.unpack $ clientTradeID order) $+$
    colName "price"                   $$ nest nestCols (text $ show $ price order) $+$
    colName "distance"                $$ nest nestCols (text $ show $ distance order) $+$
    colName "timeInForce"             $$ nest nestCols (text $ show $ timeInForce order) $+$
    colName "gtdTime"                 $$ nest nestCols (text $ show $ gtdTime order) $+$
    colName "triggerCondition"        $$ nest nestCols (text $ show $ triggerCondition order) $+$
    mVal (fillingTransactionID order) (\v -> colName "fillingTransactionID"    $$ nest nestCols (text $ T.unpack v)) $+$
    colName "filledTime"              $$ nest nestCols (text $ show $ filledTime order) $+$
    mVal (tradeOpenedID order) (\v -> colName "tradeOpenedID"           $$ nest nestCols (text $ T.unpack v)) $+$
    mVal (tradeReducedID order) (\v -> colName "tradeReducedID"          $$ nest nestCols (text $ T.unpack v)) $+$
    colName "tradeClosedIDs"          $$ nest nestCols (text $ show $ tradeClosedIDs order) $+$
    mVal (cancellingTransactionID order) (\v -> colName "cancellingTransactionID" $$ nest nestCols (text $ T.unpack v)) $+$
    colName "cancelledTime"           $$ nest nestCols (text $ show $ cancelledTime order) $+$
    mVal (replacesOrderId order) (\v -> colName "replacesOrderId"         $$ nest nestCols (text $ T.unpack v)) $+$
    mVal (replacedByOrderId order) (\v -> colName "replacedByOrderId"       $$ nest nestCols (text $ T.unpack v))

