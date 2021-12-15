-- | Currently missing endpoints:
--
-- GET  /v3/accounts/{accountID}/changes  Endpoint used to poll an Account for its current state and changes since a specified TransactionID.
-- GET  /v3/instruments/{instrument}/orderBook                               Fetch an order book for an instrument.
-- GET  /v3/instruments/{instrument}/positionBook                            Fetch a position book for an instrument.
-- GET  /v3/accounts/{accountID}/orders                                      Get a list of Orders for an Account
-- GET  /v3/accounts/{accountID}/pendingOrders                               List all pending Orders in an Account
-- GET  /v3/accounts/{accountID}/orders/{orderSpecifier}                     Get details for a single Order in an Account
-- PUT  /v3/accounts/{accountID}/orders/{orderSpecifier}                     Replace an Order in an Account by simultaneously cancelling it and creating a replacement Order
-- PUT  /v3/accounts/{accountID}/orders/{orderSpecifier}/cancel              Cancel a pending Order in an Account
-- PUT  /v3/accounts/{accountID}/orders/{orderSpecifier}/clientExtensions    Update the Client Extensions for an Order in an Account. Do not set, modify, or delete clientExtensions if your account is associated with MT4.


module Request.Oanda
    ( module R
    ) where

import           Request.Oanda.Class                     as R

import           Request.Oanda.AccountConfigurationPATCH as R
import           Request.Oanda.AccountDetailsGET         as R
import           Request.Oanda.AccountsGET               as R
import           Request.Oanda.AccountSummaryGET         as R

import           Request.Oanda.OpenTradesGET             as R

import           Request.Oanda.PositionsGET              as R

import           Request.Oanda.OrderPOST                 as R
import           Request.Oanda.OrdersGET                 as R

import           Request.Oanda.InstrumentCandlesGET      as R
import           Request.Oanda.InstrumentsGET            as R


