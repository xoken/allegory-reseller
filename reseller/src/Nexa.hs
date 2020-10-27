{-# LANGUAGE OverloadedStrings #-}

module Nexa where

import Control.Exception
import Control.Monad.IO.Unlift
import Data.Aeson
import Network.HTTP.Client
import Nexa.Auth
import Nexa.Constants
import Nexa.Types

getProducer :: (MonadUnliftIO m) => String -> SessionKey -> [Int] -> m ([Int], String, OutPoint')
getProducer addr sk name = do
    response <- liftIO $ nexaReq GetProducer (encode $ GetProducerRequest name) addr (Just sk)
    case decode (responseBody response) :: Maybe GetProducerResponse of
        Nothing -> throw ResponseParseException
        Just (GetProducerResponse name scr op) -> return (name, scr, op)

getUtxoByAddress :: (MonadIO m) => String -> SessionKey -> String -> m AddressOutputs
getUtxoByAddress nexaAddr sk addr = do
    response <- liftIO $ nexaGetReq GetUtxosByAddress addr 1 nexaAddr (Just sk)
    case decode (responseBody response) :: Maybe GetUtxosByAddressResponse of
        Nothing -> throw ResponseParseException
        Just resp -> return $ head $ utxos resp
