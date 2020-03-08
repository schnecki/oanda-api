{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.TradeOpen
    ( TradeOpen (..)

    ) where
import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics

import           Data.Oanda.AccountUnits
import           Data.Oanda.ClientExtensions
import           Data.Oanda.DecimalNumber
import           Data.Oanda.PriceValue
import           Data.Oanda.Types


-- ^ A TradeOpen object represents a Trade for an instrument that was opened in an Account. It is found embedded in
-- Transactions that affect the position of an instrument in the Account, specifically the OrderFill Transaction.
data TradeOpen = TradeOpen
  { tradeID                :: TradeId          -- ^ The ID of the Trade that was opened
  , units                  :: DecimalNumber    -- ^ The number of units opened by the Trade
  , price                  :: PriceValue       -- ^ The average price that the units were opened at.
  , guaranteedExecutionFee :: AccountUnits     -- ^ This is the fee charged for opening the trade if it has a guaranteed Stop     Loss Order attached to it.
  , clientExtensions       :: ClientExtensions -- ^ The client extensions for the newly opened Trade
  , halfSpreadCost         :: AccountUnits     -- ^ The half spread cost for the trade open. This can be a positive or     negative value and is represented in the home currency of the Account.
  , initialMarginRequired  :: AccountUnits     -- ^ The margin required at the time the Trade was created. Note, this is the     ‘pure’ margin required, it is not the ‘effective’ margin used that     factors in the trade risk if a GSLO is attached to the trade.
  } deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, NFData)

