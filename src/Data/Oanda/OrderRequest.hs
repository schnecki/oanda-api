{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Oanda.OrderRequest
  ( OrderRequest(..)
  , marketOrder
  , TimeInForceMarketOrder(..)
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text
import           Data.Time
import           GHC.Generics

import           Data.Oanda.ClientExtensions
import           Data.Oanda.DateTime
import           Data.Oanda.DecimalNumber
import           Data.Oanda.OrderPositionFill       as Fill
import           Data.Oanda.OrderState
import           Data.Oanda.OrderTriggerCondition
import           Data.Oanda.OrderType
import           Data.Oanda.PriceValue
import           Data.Oanda.StopLossDetails
import           Data.Oanda.TakeProfitDetails
import           Data.Oanda.TimeInForce
import           Data.Oanda.TrailingStopLossDetails
import           Data.Oanda.Types


-- limitOrder = LimitOrderRequest LIMIT

data TimeInForceMarketOrder
  = FOKMarketOrder -- ^ The Order must be immediately “Filled Or Killed”
  | IOCMarketOrder -- ^ The Order must be “Immediatedly paritally filled Or Cancelled”
  deriving (Eq, Ord)

fromTimeInForceMarketOrder :: TimeInForceMarketOrder -> TimeInForce
fromTimeInForceMarketOrder FOKMarketOrder = FOK
fromTimeInForceMarketOrder IOCMarketOrder = IOC

marketOrder ::
     InstrumentName         -- ^ The Market Order’s Instrument.
  -> Quantity               -- ^ The quantity requested to be filled by the Market Order. A posititive number of units
                            -- results in a long Order, and a negative number of units results in a short Order.
  -> TimeInForceMarketOrder -- ^ The time-in-force requested for the Market Order
  -> OrderRequest
marketOrder inst quantity tif = MarketOrderRequest MARKET inst quantity (fromTimeInForceMarketOrder tif) Nothing Fill.DEFAULT Nothing Nothing Nothing Nothing Nothing

data OrderRequest
  = MarketOrderRequest { orderRequestType       :: OrderType
                       , instrument             :: InstrumentName                -- ^ The Market Order’s Instrument.
                       , units                  :: DecimalNumber                 -- ^ The quantity requested to be filled by the Market Order. A posititive number of units results in a long Order, and a negative number of units results in a short Order.
                       , timeInForce            :: TimeInForce                   -- ^ The time-in-force requested for the Market Order. Restricted to FOK or IOC for a MarketOrder. [default=FOK]
                       , marketPriceBound       :: Maybe PriceValue              -- ^ The worst price that the client is willing to have the Market Order filled at.
                       , positionFill           :: OrderPositionFill             -- ^ Specification of how Positions in the Account are modified when the Order is filled. [default=DEFAULT]
                       , clientExtensions       :: Maybe ClientExtensions        -- ^ The client extensions to add to the Order. Do not set, modify, or delete clientExtensions if your account is associated with MT4.
                       , takeProfitOnFill       :: Maybe TakeProfitDetails       -- ^ TakeProfitDetails specifies the details of a TakeProfitOrder to be created on behalf of a client. This may happen when an Order is filled that opens a Trade requiring a Take Profit, or when a Trade’s dependent Take Profit Order is modified directly through the Trade.
                       , stopLossOnFill         :: Maybe StopLossDetails         -- ^ StopLossDetails specifies the details of a Stop Loss Order to be created on behalf of a client. This may happen when an Order is filled that opens a Trade requiring a Stop Loss, or when a Trade’s dependent Stop Loss Order is modified directly through the Trade.
                       , trailingStopLossOnFill :: Maybe TrailingStopLossDetails -- ^ TrailingStopLossDetails specifies the details of a Trailing Stop Loss Order to be created on behalf of a client. This may happen when an Order is filled that opens a Trade requiring a Trailing Stop Loss, or when a Trade’s dependent Trailing Stop Loss Order is modified directly through the Trade.
                       , tradeClientExtensions  :: Maybe ClientExtensions        -- ^ Client Extensions to add to the Trade created when the Order is filled (if such a Trade is created). Do not set, modify, or delete tradeClientExtensions if your account is associated with MT4.
                        }
  | LimitOrderRequest { orderRequestType       :: OrderType
                      , instrument             :: InstrumentName                -- ^ The Limit Order’s Instrument.
                      , units                  :: DecimalNumber                 -- ^ The quantity requested to be filled by the Limit Order. A posititive number of units results in a long Order, and a negative number of units results in a short Order.
                      , price                  :: PriceValue                    -- ^ The price threshold specified for the Limit Order. The Limit Order will only be filled by a market price that is equal to or better than this price.
                      , timeInForce            :: TimeInForce                   -- ^ The time-in-force requested for the Limit Order. [default=GTC]
                      , gtdTime                :: Maybe DateTime                -- ^ The date/time when the Limit Order will be cancelled if its timeInForce     is “GTD”.
                      , positionFill           :: OrderPositionFill             -- ^ Specification of how Positions in the Account are modified when the Order     is filled. [default=DEFAULT]
                      , triggerCondition       :: OrderTriggerCondition         -- ^ Specification of which price component should be used when determining if     an Order should be triggered and filled. This allows Orders to be     triggered based on the bid, ask, mid, default (ask for buy, bid for sell)     or inverse (ask for sell, bid for buy) price depending on the desired     behaviour. Orders are always filled using their default price component.     This feature is only provided through the REST API. Clients who choose to     specify a non-default trigger condition will not see it reflected in any     of OANDA’s proprietary or partner trading platforms, their transaction     history or their account statements. OANDA platforms always assume that     an Order’s trigger condition is set to the default value when indicating     the distance from an Order’s trigger price, and will always provide the     default trigger condition when creating or modifying an Order. A special     restriction applies when creating a guaranteed Stop Loss Order. In this     case the TriggerCondition value must either be “DEFAULT”, or the     “natural” trigger side “DEFAULT” results in. So for a Stop Loss Order for     a long trade valid values are “DEFAULT” and “BID”, and for short trades     “DEFAULT” and “ASK” are valid. [default=DEFAULT]
                      , clientExtensions       :: Maybe ClientExtensions        -- ^ The client extensions to add to the Order. Do not set, modify, or delete     clientExtensions if your account is associated with MT4.
                      , takeProfitOnFill       :: Maybe TakeProfitDetails       -- ^ TakeProfitDetails specifies the details of a Take Profit Order to be     created on behalf of a client. This may happen when an Order is filled     that opens a Trade requiring a Take Profit, or when a Trade’s dependent     Take Profit Order is modified directly through the Trade.
                      , stopLossOnFill         :: Maybe StopLossDetails         -- ^ StopLossDetails specifies the details of a Stop Loss Order to be created     on behalf of a client. This may happen when an Order is filled that opens     a Trade requiring a Stop Loss, or when a Trade’s dependent Stop Loss     Order is modified directly through the Trade.
                      , trailingStopLossOnFill :: Maybe TrailingStopLossDetails -- ^ TrailingStopLossDetails specifies the details of a Trailing Stop Loss     Order to be created on behalf of a client. This may happen when an Order     is filled that opens a Trade requiring a Trailing Stop Loss, or when a     Trade’s dependent Trailing Stop Loss Order is modified directly through     the Trade.
                      , tradeClientExtensions  :: Maybe ClientExtensions        -- ^ Client Extensions to add to the Trade created when the Order is filled     (if such a Trade is created). Do not set, modify, or delete     tradeClientExtensions if your account is associated with MT4.
                       }
  | StopOrderRequest { orderRequestType       :: OrderType
                     , instrument             :: InstrumentName -- ^ The Stop Order’s Instrument.
                     , units                  :: DecimalNumber -- ^ The quantity requested to be filled by the Stop Order. A posititive     number of units results in a long Order, and a negative number of units     results in a short Order.
                     , price                  :: PriceValue -- ^ The price threshold specified for the Stop Order. The Stop Order will     only be filled by a market price that is equal to or worse than this     price.
                     , priceBound             :: Maybe PriceValue -- ^ The worst market price that may be used to fill this Stop Order. If the     market gaps and crosses through both the price and the priceBound, the     Stop Order will be cancelled instead of being filled.
                     , timeInForce            :: TimeInForce -- ^ The time-in-force requested for the Stop Order. [default=GTC]
                     , gtdTime                :: Maybe DateTime -- ^ The date/time when the Stop Order will be cancelled if its timeInForce is     “GTD”.
                     , positionFill           :: OrderPositionFill -- ^ Specification of how Positions in the Account are modified when the Order     is filled. [default=DEFAULT]
                     , triggerCondition       :: OrderTriggerCondition -- ^ Specification of which price component should be used when determining if     an Order should be triggered and filled. This allows Orders to be     triggered based on the bid, ask, mid, default (ask for buy, bid for sell)     or inverse (ask for sell, bid for buy) price depending on the desired     behaviour. Orders are always filled using their default price component.     This feature is only provided through the REST API. Clients who choose to     specify a non-default trigger condition will not see it reflected in any     of OANDA’s proprietary or partner trading platforms, their transaction     history or their account statements. OANDA platforms always assume that     an Order’s trigger condition is set to the default value when indicating     the distance from an Order’s trigger price, and will always provide the     default trigger condition when creating or modifying an Order. A special     restriction applies when creating a guaranteed Stop Loss Order. In this     case the TriggerCondition value must either be “DEFAULT”, or the     “natural” trigger side “DEFAULT” results in. So for a Stop Loss Order for     a long trade valid values are “DEFAULT” and “BID”, and for short trades     “DEFAULT” and “ASK” are valid. [default=DEFAULT]
                     , clientExtensions       :: Maybe ClientExtensions -- ^ The client extensions to add to the Order. Do not set, modify, or delete     clientExtensions if your account is associated with MT4.
                     , takeProfitOnFill       :: Maybe TakeProfitDetails -- ^ TakeProfitDetails specifies the details of a Take Profit Order to be     created on behalf of a client. This may happen when an Order is filled     that opens a Trade requiring a Take Profit, or when a Trade’s dependent     Take Profit Order is modified directly through the Trade.
                     , stopLossOnFill         :: Maybe StopLossDetails -- ^ StopLossDetails specifies the details of a Stop Loss Order to be created     on behalf of a client. This may happen when an Order is filled that opens     a Trade requiring a Stop Loss, or when a Trade’s dependent Stop Loss     Order is modified directly through the Trade.
                     , trailingStopLossOnFill :: Maybe TrailingStopLossDetails -- ^ TrailingStopLossDetails specifies the details of a Trailing Stop Loss     Order to be created on behalf of a client. This may happen when an Order     is filled that opens a Trade requiring a Trailing Stop Loss, or when a     Trade’s dependent Trailing Stop Loss Order is modified directly through     the Trade.
                     , tradeClientExtensions  :: Maybe ClientExtensions -- ^ Client Extensions to add to the Trade created when the Order is filled     (if such a Trade is created). Do not set, modify, or delete     tradeClientExtensions if your account is associated with MT4.
                      }
  | MarketIfTouchedOrderRequest { orderRequestType       :: OrderType
                                , instrument             :: InstrumentName -- ^ The MarketIfTouched Order’s Instrument.
                                , units                  :: DecimalNumber -- ^ The quantity requested to be filled by the MarketIfTouched Order. A     posititive number of units results in a long Order, and a negative number     of units results in a short Order.
                                , price                  :: PriceValue -- ^ The price threshold specified for the MarketIfTouched Order. The     MarketIfTouched Order will only be filled by a market price that crosses     this price from the direction of the market price at the time when the     Order was created (the initialMarketPrice). Depending on the value of the     Order’s price and initialMarketPrice, the MarketIfTouchedOrder will     behave like a Limit or a Stop Order.
                                , priceBound             :: Maybe PriceValue -- ^ The worst market price that may be used to fill this MarketIfTouched     Order.
                                , timeInForce            :: TimeInForce -- ^ The time-in-force requested for the MarketIfTouched Order. Restricted to     “GTC”, “GFD” and “GTD” for MarketIfTouched Orders. [default=GTC]
                                , gtdTime                :: Maybe DateTime -- ^ The date/time when the MarketIfTouched Order will be cancelled if its     timeInForce is “GTD”.
                                , positionFill           :: OrderPositionFill -- ^ Specification of how Positions in the Account are modified when the Order     is filled. [default=DEFAULT]
                                , triggerCondition       :: OrderTriggerCondition -- ^ Specification of which price component should be used when determining if     an Order should be triggered and filled. This allows Orders to be     triggered based on the bid, ask, mid, default (ask for buy, bid for sell)     or inverse (ask for sell, bid for buy) price depending on the desired     behaviour. Orders are always filled using their default price component.     This feature is only provided through the REST API. Clients who choose to     specify a non-default trigger condition will not see it reflected in any     of OANDA’s proprietary or partner trading platforms, their transaction     history or their account statements. OANDA platforms always assume that     an Order’s trigger condition is set to the default value when indicating     the distance from an Order’s trigger price, and will always provide the     default trigger condition when creating or modifying an Order. A special     restriction applies when creating a guaranteed Stop Loss Order. In this     case the TriggerCondition value must either be “DEFAULT”, or the     “natural” trigger side “DEFAULT” results in. So for a Stop Loss Order for     a long trade valid values are “DEFAULT” and “BID”, and for short trades     “DEFAULT” and “ASK” are valid. [default=DEFAULT]
                                , clientExtensions       :: Maybe ClientExtensions -- ^ The client extensions to add to the Order. Do not set, modify, or delete     clientExtensions if your account is associated with MT4.
                                , takeProfitOnFill       :: Maybe TakeProfitDetails -- ^ TakeProfitDetails specifies the details of a Take Profit Order to be     created on behalf of a client. This may happen when an Order is filled     that opens a Trade requiring a Take Profit, or when a Trade’s dependent     Take Profit Order is modified directly through the Trade.
                                , stopLossOnFill         :: Maybe StopLossDetails -- ^ StopLossDetails specifies the details of a Stop Loss Order to be created     on behalf of a client. This may happen when an Order is filled that opens     a Trade requiring a Stop Loss, or when a Trade’s dependent Stop Loss     Order is modified directly through the Trade.
                                , trailingStopLossOnFill :: Maybe TrailingStopLossDetails -- ^ TrailingStopLossDetails specifies the details of a Trailing Stop Loss     Order to be created on behalf of a client. This may happen when an Order     is filled that opens a Trade requiring a Trailing Stop Loss, or when a     Trade’s dependent Trailing Stop Loss Order is modified directly through     the Trade.
                                , tradeClientExtensions  :: Maybe ClientExtensions -- ^ Client Extensions to add to the Trade created when the Order is filled     (if such a Trade is created). Do not set, modify, or delete     tradeClientExtensions if your account is associated with MT4.
                                 }
  | TakeProfitOrderRequest { orderRequestType :: OrderType
                           , tradeID          :: TradeId -- ^ The ID of the Trade to close when the price threshold is breached.
                           , clientTradeID    :: Maybe ClientId -- ^ The client ID of the Trade to be closed when the price threshold is     breached.
                           , price            :: PriceValue -- ^ The price threshold specified for the TakeProfit Order. The associated     Trade will be closed by a market price that is equal to or better than     this threshold.
                           , timeInForce      :: TimeInForce -- ^ The time-in-force requested for the TakeProfit Order. Restricted to     “GTC”, “GFD” and “GTD” for TakeProfit Orders.  [default=GTC]
                           , gtdTime          :: Maybe DateTime -- ^ The date/time when the TakeProfit Order will be cancelled if its     timeInForce is “GTD”.
                           , triggerCondition :: OrderTriggerCondition -- ^ Specification of which price component should be used when determining if     an Order should be triggered and filled. This allows Orders to be     triggered based on the bid, ask, mid, default (ask for buy, bid for sell)     or inverse (ask for sell, bid for buy) price depending on the desired     behaviour. Orders are always filled using their default price component.     This feature is only provided through the REST API. Clients who choose to     specify a non-default trigger condition will not see it reflected in any     of OANDA’s proprietary or partner trading platforms, their transaction     history or their account statements. OANDA platforms always assume that     an Order’s trigger condition is set to the default value when indicating     the distance from an Order’s trigger price, and will always provide the     default trigger condition when creating or modifying an Order. A special     restriction applies when creating a guaranteed Stop Loss Order. In this     case the TriggerCondition value must either be “DEFAULT”, or the     “natural” trigger side “DEFAULT” results in. So for a Stop Loss Order for     a long trade valid values are “DEFAULT” and “BID”, and for short trades     “DEFAULT” and “ASK” are valid. [default=DEFAULT]
                           , clientExtensions :: Maybe ClientExtensions -- ^ The client extensions to add to the Order. Do not set, modify, or delete     clientExtensions if your account is associated with MT4.
                            }
  | StopLossOrderRequest { orderRequestType :: OrderType
                         , tradeID          :: TradeId -- ^ The ID of the Trade to close when the price threshold is breached.
                         , clientTradeID    :: Maybe ClientId -- ^ The client ID of the Trade to be closed when the price threshold is     breached.
                         , price            :: PriceValue -- ^ The price threshold specified for the Stop Loss Order. If the guaranteed     flag is false, the associated Trade will be closed by a market price that     is equal to or worse than this threshold. If the flag is true the     associated Trade will be closed at this price.
                         , distance         :: Maybe DecimalNumber -- ^ Specifies the distance (in price units) from the Account’s current price     to use as the Stop Loss Order price. If the Trade is short the     Instrument’s bid price is used, and for long Trades the ask is used.
                         , timeInForce      :: TimeInForce -- ^ The time-in-force requested for the StopLoss Order. Restricted to “GTC”,     “GFD” and “GTD” for StopLoss Orders. [default=GTC]
                         , gtdTime          :: Maybe DateTime -- ^ The date/time when the StopLoss Order will be cancelled if its     timeInForce is “GTD”.
                         , triggerCondition :: OrderTriggerCondition -- ^ Specification of which price component should be used when determining if     an Order should be triggered and filled. This allows Orders to be     triggered based on the bid, ask, mid, default (ask for buy, bid for sell)     or inverse (ask for sell, bid for buy) price depending on the desired     behaviour. Orders are always filled using their default price component.     This feature is only provided through the REST API. Clients who choose to     specify a non-default trigger condition will not see it reflected in any     of OANDA’s proprietary or partner trading platforms, their transaction     history or their account statements. OANDA platforms always assume that     an Order’s trigger condition is set to the default value when indicating     the distance from an Order’s trigger price, and will always provide the     default trigger condition when creating or modifying an Order. A special     restriction applies when creating a guaranteed Stop Loss Order. In this     case the TriggerCondition value must either be “DEFAULT”, or the     “natural” trigger side “DEFAULT” results in. So for a Stop Loss Order for     a long trade valid values are “DEFAULT” and “BID”, and for short trades     “DEFAULT” and “ASK” are valid. [default=DEFAULT]
                         , guaranteed       :: Bool -- ^ Flag indicating that the Stop Loss Order is guaranteed. The default value     depends on the GuaranteedStopLossOrderMode of the account, if it is     REQUIRED, the default will be true, for DISABLED or ENABLED the default     is false.
                         , clientExtensions :: Maybe ClientExtensions -- ^ The client extensions to add to the Order. Do not set, modify, or delete     clientExtensions if your account is associated with MT4.
                          }
  | TrailingStopLossOrderRequest { orderRequestType :: OrderType
                                 , tradeID          :: TradeId -- ^ The ID of the Trade to close when the price threshold is breached.
                                 , clientTradeID    :: Maybe ClientId -- ^ The client ID of the Trade to be closed when the price threshold is     breached.
                                 , distance         :: Maybe DecimalNumber -- ^ The price distance (in price units) specified for the TrailingStopLoss     Order.
                                 , timeInForce      :: TimeInForce -- ^ The time-in-force requested for the TrailingStopLoss Order. Restricted to     “GTC”, “GFD” and “GTD” for TrailingStopLoss Orders. [default=GTC]
                                 , gtdTime          :: Maybe DateTime -- ^ The date/time when the StopLoss Order will be cancelled if its     timeInForce is “GTD”.
                                 , triggerCondition :: OrderTriggerCondition -- ^ Specification of which price component should be used when determining if     an Order should be triggered and filled. This allows Orders to be     triggered based on the bid, ask, mid, default (ask for buy, bid for sell)     or inverse (ask for sell, bid for buy) price depending on the desired     behaviour. Orders are always filled using their default price component.     This feature is only provided through the REST API. Clients who choose to     specify a non-default trigger condition will not see it reflected in any     of OANDA’s proprietary or partner trading platforms, their transaction     history or their account statements. OANDA platforms always assume that     an Order’s trigger condition is set to the default value when indicating     the distance from an Order’s trigger price, and will always provide the     default trigger condition when creating or modifying an Order. A special     restriction applies when creating a guaranteed Stop Loss Order. In this     case the TriggerCondition value must either be “DEFAULT”, or the     “natural” trigger side “DEFAULT” results in. So for a Stop Loss Order for     a long trade valid values are “DEFAULT” and “BID”, and for short trades     “DEFAULT” and “ASK” are valid. [default=DEFAULT]
                                 , clientExtensions :: Maybe ClientExtensions -- ^ The client extensions to add to the Order. Do not set, modify, or delete     clientExtensions if your account is associated with MT4.
                                  }
  deriving (Show, Eq, Ord, FromJSON, Generic, NFData)

-- Derive from JSON by modifying the NAV-nav field.
$(deriveToJSON
    defaultOptions
      { omitNothingFields = True,
        fieldLabelModifier =
          let f "orderRequestType" = "type"
              f "marketPriceBound" = "priceBound"
              f x                  = x
           in f
      }
    ''OrderRequest)
