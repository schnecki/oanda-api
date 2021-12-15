{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
module Request.Oanda.Class
  ( Request(..)
  , Session(..)
  , OandaConfig (..)
  , oandaConfigPracticeAccount
  , oandaConfigTradeAccount
  -- , baseUrl
  , additionalParams
  , headerContentTypeJson
  , headerContentDispositionFile
  , headerContentTypeMultipart
  , headerRFC3339DatetimeFormat
  , runSafeReqM
  , SafeReqM (..)
  ) where

import qualified Data.ByteString.Char8 as B
import           Data.Text             (Text)

import           ApiMaker

version :: Text
version = "v3"

type AccessToken = B.ByteString

oandaConfigPracticeAccount :: AccessToken -> OandaConfig
oandaConfigPracticeAccount = OandaConfig baseUrlPractice streamUrlPractice

oandaConfigTradeAccount :: AccessToken -> OandaConfig
oandaConfigTradeAccount = OandaConfig baseUrlTrade streamUrlTrade


data OandaConfig = OandaConfig
  { baseUrl     :: Url 'Https
  , streamUrl   :: Url 'Https
  , accessToken :: AccessToken
  }


additionalParams :: OandaConfig -> [Option 'Https]
additionalParams config = [headerBearer config]

baseUrlTrade :: Url 'Https
baseUrlTrade = https "api-fxtrade.oanda.com" /: version

baseUrlPractice :: Url 'Https
baseUrlPractice = https "api-fxpractice.oanda.com" /: version

streamUrlTrade :: Url 'Https
streamUrlTrade = https "stream-fxtrade.oanda.com" /: version

streamUrlPractice :: Url 'Https
streamUrlPractice = https "stream-fxpractice.oanda.com" /: version

-- TODO: see http://hdiff.luite.com/cgit/oanda-rest-api/commit?id=0.4.0 for streaming

headerBearer :: OandaConfig -> Option 'Https
headerBearer config = header "Authorization" ("Bearer " <> accessToken config)

headerRFC3339DatetimeFormat :: Option 'Https
headerRFC3339DatetimeFormat = header "Accept-Datetime-Format" "RFC3339"

