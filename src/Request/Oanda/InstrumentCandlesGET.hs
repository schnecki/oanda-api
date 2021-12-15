{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Oanda.InstrumentCandlesGET
  ( GetInstrumentCandle (..)
  , CandleConfig (..)
  , CandlestickGranularity (..)
  ) where

import           ApiMaker
import           Data.Text                         (Text)

import           Data.Oanda.Candles
import           Data.Oanda.CandlestickGranularity
import           Data.Oanda.DateTime
import           Data.Oanda.Types
import           Data.Oanda.WeeklyAlignment
import           Request.Oanda.Class


data GetInstrumentCandle = GetInstrumentCandle InstrumentName CandleConfig

data CandleConfig = CandleConfig
  { price             :: Maybe Text                   -- ^ The Price component(s) to get candlestick data for. Can contain any combination of the characters “M” (midpoint candles) “B” (bid candles) and “A” (ask candles). [default=M]                                                                                                                                      |
  , granularity       :: Maybe CandlestickGranularity -- ^ The granularity of the candlesticks to fetch [default=S5]                                                                                                                                                                                                                                                          |
  , count             :: Maybe Int                    -- ^ The number of candlesticks to return in the reponse. Count should not be specified if both the start and end parameters are provided, as the time range combined with the graularity will determine the number of candlesticks to return. [default=500, maximum=5000]                                              |
  , from              :: Maybe DateTime               -- ^ The start of the time range to fetch candlesticks for.                                                                                                                                                                                                                                                             |
  , to                :: Maybe DateTime               -- ^ The end of the time range to fetch candlesticks for.                                                                                                                                                                                                                                                               |
  , smooth            :: Maybe Bool                   -- ^ A flag that controls whether the candlestick is “smoothed” or not. A smoothed candlestick uses the previous candle’s close price as its open price, while an unsmoothed candlestick uses the first price from its time range as its open price. [default=False]                                                    |
  , includeFirst      :: Maybe Bool                   -- ^ A flag that controls whether the candlestick that is covered by the from time should be included in the results. This flag enables clients to use the timestamp of the last completed candlestick received to poll for future candlesticks but avoid receiving the previous candlestick repeatedly. [default=True] |
  , dailyAlignment    :: Maybe Int                    -- ^ The hour of the day (in the specified timezone) to use for granularities that have daily alignments. [default=17, minimum=0, maximum=23]                                                                                                                                                                           |
  , alignmentTimezone :: Maybe Text                   -- ^ The timezone to use for the dailyAlignment parameter. Candlesticks with daily alignment will be aligned to the dailyAlignment hour within the alignmentTimezone. Note that the returned times will still be represented in UTC. [default=America/New_York]                                                         |
  , weeklyAlignment   :: Maybe WeeklyAlignment        -- ^ The day of the week used for granularities that have weekly alignment. [default=Friday]                                                                                                                                                                                                                            |
  } deriving (Show)


instance Request OandaConfig GetInstrumentCandle where
  type Method GetInstrumentCandle = GET
  type Body GetInstrumentCandle = NoReqBody
  type Response GetInstrumentCandle = JsonResponse Candles
  type Output GetInstrumentCandle = Candles
  method _ GetInstrumentCandle {} = GET
  url cfg (GetInstrumentCandle instrument _) = baseUrl cfg /: "instruments" /: instrument /: "candles"
  body _ GetInstrumentCandle {} = NoReqBody
  response _ GetInstrumentCandle {} = jsonResponse
  option _ (GetInstrumentCandle _ cfg) = return $ headerRFC3339DatetimeFormat <> configs
    where
      configs =
        case cfg of
          CandleConfig p g c f t s i d a w ->
             "price"             `maybeQueryParam` p           <>
             "granularity"       `maybeQueryParam` fmap show g <>
             "count"             `maybeQueryParam` c           <>
             "from"              `maybeQueryParam` fmap show f <>
             "to"                `maybeQueryParam` fmap show t <>
             "smooth"            `maybeQueryParam` s           <>
             "includeFirst"      `maybeQueryParam` i           <>
             "dailyAlignment"    `maybeQueryParam` d           <>
             "alignmentTimezone" `maybeQueryParam` a           <>
             "weeklyAlignment"   `maybeQueryParam` fmap show w
  process _ GetInstrumentCandle {} response = return $ responseBody response
