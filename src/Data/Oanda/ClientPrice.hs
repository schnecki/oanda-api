{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Oanda.ClientPrice
    ( ClientPrice (..)
    , prettyClientPrice
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Oanda.DateTime
import           Data.Oanda.PriceBucket
import           Data.Oanda.PriceValue
import           Data.Oanda.Types


data ClientPrice = ClientPrice
  { bids        :: [PriceBucket] -- ^ The list of prices and liquidity available on the Instrument’s bid side. It is possible for this list to be empty if there is no bid liquidity currently available for the Instrument in the Account.
  , asks        :: [PriceBucket] -- ^ The list of prices and liquidity available on the Instrument’s ask side. It is possible for this list to be empty if there is no ask liquidity currently available for the Instrument in the Account.
  , closeoutBid :: PriceValue    -- ^ The closeout bid Price. This Price is used when a bid is required to closeout a Position (margin closeout or manual) yet there is no bid liquidity. The closeout bid is never used to open a new position.
  , closeoutAsk :: PriceValue    -- ^ The closeout ask Price. This Price is used when a ask is required to closeout a Position (margin closeout or manual) yet there is no ask liquidity. The closeout ask is never used to open a new position.
  , timestamp   :: DateTime      -- ^ The date/time when the Price was created.
  } deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, NFData)


prettyClientPrice :: ClientPrice -> Doc
prettyClientPrice clientPrice =
  colName "bids"        $$ nest nestCols (vcat $ map prettyPriceBucket $ bids clientPrice) $+$ -- [PriceBucket]
  colName "asks"        $$ nest nestCols (vcat $ map prettyPriceBucket $ asks clientPrice) $+$ -- [PriceBucket]
  colName "closeoutBid" $$ nest nestCols (prettyPriceValue $ closeoutBid clientPrice) $+$      -- PriceValue
  colName "closeoutAsk" $$ nest nestCols (prettyPriceValue $ closeoutAsk clientPrice) $+$      -- PriceValue
  colName "timestamp"   $$ nest nestCols (text $ show $ timestamp clientPrice)                 -- DateTime


