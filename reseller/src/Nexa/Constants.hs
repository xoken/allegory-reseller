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
    | GetProducer
    | GetUtxosByAddress
    deriving (Show, Eq, Read)

noCertValidationManager :: IO Manager
noCertValidationManager = newManager $ mkManagerSettings (TLSSettingsSimple True False False) Nothing

nexaPostEndpoint :: String -> NexaEndpoint -> String
nexaPostEndpoint addr ep =
    "https://" <> addr <> "/v1/" <>
    case ep of
        Auth -> "auth"
        GetProducer -> "producer"

nexaGetEndpoint :: String -> NexaEndpoint -> String -> Int -> String
nexaGetEndpoint addr ep qstr pgSize =
    "https://" <> addr <> "/v1/" <>
    case ep of
        GetUtxosByAddress -> "address/" <> qstr <> "/utxos/?pagesize=" <> (show pgSize)

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

nexaGetReq :: NexaEndpoint -> String -> Int -> String -> Maybe SessionKey -> IO (Response L.ByteString)
nexaGetReq ep qstr pgSize addr sk = do
    man <- noCertValidationManager
    init <- parseRequest $ nexaGetEndpoint addr ep qstr pgSize
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
