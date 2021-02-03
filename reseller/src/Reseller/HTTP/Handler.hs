{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reseller.HTTP.Handler where

import Control.Applicative ((<|>))
import qualified Control.Error.Util as Extra
import Control.Exception (SomeException(..), throw, try)
import qualified Control.Exception.Lifted as LE (try)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.State.Class
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Either as Either
import qualified Data.HashTable.IO as H
import Data.Int
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Serialize as S
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Reseller.Common
import Reseller.Env
import Reseller.HTTP.Types
import Reseller.Service.Xoken
import Snap
import qualified System.Logger as LG

getPartiallySignedAllegoryTx :: ReqParams' -> Handler App App ()
getPartiallySignedAllegoryTx GetPartiallySignedAllegoryTx {..} = do
    pretty <- (maybe True (read . DT.unpack . DTE.decodeUtf8)) <$> (getQueryParam "pretty")
    bp2pEnv <- getBitcoinP2P
    res <-
        LE.try $
        xGetPartiallySignedAllegoryTx
            (nodeConfig bp2pEnv)
            gpsaPaymentInputs
            gpsaName
            gpsaOutputOwner
            gpsaOutputChange
            gpsaBuyerUri
    case res of
        Left (e :: SomeException) -> do
            modifyResponse $ setResponseStatus 500 "Internal Server Error"
            writeBS "INTERNAL_SERVER_ERROR"
        Right ops -> do
            writeBS $ BSL.toStrict $ encodeResp pretty $ RespPartiallySignedAllegoryTx ops
getPartiallySignedAllegoryTx _ = throwBadRequest

withReq :: Aeson.FromJSON a => (a -> Handler App App ()) -> Handler App App ()
withReq handler = do
    rq <- getRequest
    let ct = getHeader "content-type" rq <|> (getHeader "Content-Type" rq) <|> (getHeader "Content-type" rq)
    if ct == Just "application/json"
        then do
            bsReq <- readRequestBody (8 * 2048)
            case Aeson.eitherDecode bsReq of
                Right r -> handler r
                Left err -> do
                    modifyResponse $ setResponseStatus 400 "Bad Request"
                    writeBS "400 error"
        else throwBadRequest

throwDenied :: Handler App App ()
throwDenied = do
    modifyResponse $ setResponseStatus 403 "Access Denied"
    writeBS "Access Denied"

throwBadRequest :: Handler App App ()
throwBadRequest = do
    modifyResponse $ setResponseStatus 400 "Bad Request"
    writeBS "Bad Request"

throwNotFound :: Handler App App ()
throwNotFound = do
    modifyResponse $ setResponseStatus 404 "Not Found"
    writeBS "Not Found"
