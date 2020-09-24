{-# LANGUAGE OverloadedStrings #-}

module Nexa.Auth where

import Data.Aeson
import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Nexa.Constants
import Nexa.Types

getNexaSessionKey :: String -> String -> String -> IO (Either String SessionKey)
getNexaSessionKey nexaAddr user pass = do
    response <- nexaReq Auth (encode $ AuthenticateRequest user pass) nexaAddr Nothing
    case decode (responseBody response) :: Maybe AuthenticateResponse of
        Nothing -> return $ Left "bad request: check host config"
        Just (AuthenticateResponse a) -> do
            case sessionKey a of
                Nothing -> return $ Left "incorrect username/password"
                Just s -> return $ Right s
