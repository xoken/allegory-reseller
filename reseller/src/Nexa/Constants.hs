{-# LANGUAGE OverloadedStrings #-}

module Nexa.Constants where

import Data.ByteString.Char8 as C
import Data.ByteString.Lazy as L
import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.TLS

type SessionKey = String

data NexaEndpoint
    = Auth
    | NameOutpoint
    | GetUtxosByAddress
    | RelayTransaction
    deriving (Show, Eq, Read)

noCertValidationManager :: IO Manager
noCertValidationManager = newManager $ mkManagerSettings (TLSSettingsSimple True False False) Nothing

nexaPostEndpoint :: String -> NexaEndpoint -> String
nexaPostEndpoint addr ep =
    "https://" <> addr <> "/v1/" <>
    case ep of
        Auth -> "auth"
        NameOutpoint -> "name-outpoint"
        RelayTransaction -> "relay"

nexaGetEndpoint :: String -> NexaEndpoint -> String -> Int -> String
nexaGetEndpoint addr ep qstr pgSize =
    "https://" <> addr <> "/v1/" <>
    case ep of
        GetUtxosByAddress -> "address/" <> qstr <> "/utxos/?pagesize=" <> (show pgSize)

utxosByAddressRequest :: String -> String -> Int -> Maybe String -> String
utxosByAddressRequest nexaEndpoint address pageSize cursor =
    let ep = "https://" <> nexaEndpoint <> "/v1/"
        rq = "address/" <> address <> "/utxos/"
        pg = "?pagesize=" <> (show pageSize)
        cu =
            case cursor of
                Nothing -> ""
                Just c' -> "&cursor=" <> c'
     in ep <> rq <> pg <> cu

nexaReq :: NexaEndpoint -> L.ByteString -> String -> Maybe SessionKey -> IO (Response L.ByteString)
nexaReq ep body addr sk = do
    man <- noCertValidationManager
    init <- parseRequest $ nexaPostEndpoint addr ep
    let req =
            init
                { method = "POST"
                , requestHeaders =
                      ("content-type", "application/json") :
                      ((\k ->
                            case k of
                                Nothing -> []
                                Just k' -> [("Authorization", "Bearer " <> k')]) $
                       C.pack <$> sk)
                , requestBody = RequestBodyLBS body
                }
    httpLbs req man

nexaGetReq :: String -> Maybe SessionKey -> IO (Response L.ByteString)
nexaGetReq req sk = do
    man <- noCertValidationManager
    init <- parseRequest $ req
    let req =
            init
                { method = "GET"
                , requestHeaders =
                      ((\k ->
                            case k of
                                Nothing -> []
                                Just k' -> [("Authorization", "Bearer " <> k')]) $
                       C.pack <$> sk)
                }
    httpLbs req man
