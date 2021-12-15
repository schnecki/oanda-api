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

module OandaApi
    ( module Oanda
    ) where

import           ApiMaker         as Oanda
import           Data.Oanda.Types as Oanda
import           Request.Oanda    as Oanda
