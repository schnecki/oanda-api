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
module Request.Class
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
  , runReqSafe
  , ReqSafe (..)
  ) where

import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8       as B
import           Data.Proxy
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as E
import qualified Network.HTTP.Client         as C

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

headerContentTypeJson :: Option 'Https
headerContentTypeJson = header "content-type" "application/json"

headerContentTypeMultipart :: Option 'Https
headerContentTypeMultipart = header "content-type" "multipart/form-data"

headerContentDispositionFile :: Text -> Option 'Https
headerContentDispositionFile filename = header "Content-Disposition" (E.encodeUtf8 $ T.concat ["attachment; filename=\"", filename, "\""])


