{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                (forM, forM_, void)
import           Control.Monad.Trans          (liftIO)
import           Control.Monad.Trans.State
import           Data.Aeson                   (encode)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as C
import qualified Data.Word                    as W8
import           Prelude                      hiding (id)

import           ApiMaker

import           Data.Oanda.AccountProperties
import           Data.Oanda.Accounts
import           Data.Oanda.Instrument
import           Data.Oanda.Instruments
import           Data.Oanda.OrderRequest
import           Data.Oanda.OrderRequest
import           Request

main :: IO ()
main = do
  apiKey <- C.filter (/= '\n') <$> B.readFile "API_KEY"
  putStrLn $ "API_KEY: " <> show apiKey
  let cfg = oandaConfigPracticeAccount apiKey
  res <-
    runSessReqWithParamsM (additionalParams cfg) cfg $ runRequests $ do
      accs <- mkReq GetAccounts
      liftIO $ print accs
      forM_ (accounts accs) $ \prop -> do
        let accId = id prop
        res <- mkReq $ GetAccountDetails accId
        liftIO $ putStrLn $ take 100 $ show res
        res <- mkReq $ GetAccountSummary accId
        liftIO $ putStrLn $ take 100 $ show res
        insts <- mkReq $ GetInstruments accId Nothing
        liftIO $ putStrLn $ take 100 $ show insts
        let config = AccountConfigurationUpdate (Just "Test Account Nr uno") (Just 1)
        liftIO $ print $ "Trying to set following values: " <> encode config
        res <- mkReq $ PatchAccountConfiguration accId config
        liftIO $ putStrLn $ take 100 $ show res
        let candleCfg = CandleConfig (Just "M") (Just S5) (Just 6) Nothing Nothing Nothing Nothing Nothing Nothing Nothing -- count=6&price=M&granularity=S5
        res <- mkReq $ GetInstrumentCandle "EUR_USD" candleCfg
        liftIO $ putStrLn $ take 100 $ show res
        let orderReq = marketOrder "EUR_USD" 1.0 FOKMarketOrder 0.8
        liftIO $ print $ encode orderReq
      -- res <- mkReq $ PostOrder accId orderReq
      -- liftIO $ print res
        res <- mkReq $ GetAccountSummary accId
        liftIO $ print res
  print res
