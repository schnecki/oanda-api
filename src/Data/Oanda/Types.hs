module Data.Oanda.Types where

import           Data.Text        (Text)
import           Prelude          hiding ((<>))
import           Text.PrettyPrint

type AccountId = Text
type TransactionId = Text
type TradeId = Text
type ClientId = Text
type OrderId = Text -- ^ The string representation of the OANDA-assigned OrderID. OANDA-assigned OrderIDs are positive
                    -- integers, and are derived from the TransactionID of the Transaction that created the Order.
type RequestId = Text

type ClientComment = Text

type InstrumentName = Text      -- ^ A string containing the base currency and quote currency delimited by a “_”.

type ClientTag = Text           -- ^ A client-provided tag that can contain any data and may be assigned to their Orders
                                -- or Trades. Tags are typically used to associate groups of Trades and/or Orders
                                -- together.

type Percentage = Text          -- TODO
type PositionValue = Text       -- TODO


type MarginRate = Text          -- TODO make Double


nestCols :: Int
nestCols = 65


colName :: String -> Doc
colName n = text n <> colon

-- mVal :: Maybe
mVal :: Maybe t -> (t -> Doc) -> Doc
mVal Nothing _     = mempty
mVal (Just v) line = line v

mDefVal :: Doc -> Maybe t -> (t -> Doc) -> Doc
mDefVal def Nothing _   = def
mDefVal _ (Just v) line = line v

