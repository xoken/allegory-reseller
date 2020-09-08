{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Reseller.Service.Xoken where

import Control.Exception
import qualified Control.Exception.Lifted as LE (try)
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as DT
import Network.HTTP.Req
import NodeConfig
import Prelude
import Reseller.HTTP.Types

xGetPartiallySignedAllegoryTx ::
       (MonadIO m, MonadHttp m, MonadBaseControl IO m)
    => NodeConfig
    -> [(OutPoint', Int)]
    -> ([Int], Bool)
    -> (String)
    -> (String)
    -> m (BC.ByteString)
xGetPartiallySignedAllegoryTx nodeCnf payips np owner change = do
    resp <-
        req
            POST
            (https (DT.pack $ xokenListenIP nodeCnf))
            (ReqBodyJson (GetPartiallySignedAllegoryTx payips np owner change))
            bsResponse
            ((port $ fromEnum $ xokenListenPort nodeCnf) <> mempty)
    case eitherDecodeStrict $ responseBody resp of
        Right (RespPartiallySignedAllegoryTx pis)
          -- for the intermediate tx, call relayTx (parallel)
          -- return the final partially signed tx
         -> do
            undefined
        Left err -> undefined

xRelayTx :: (MonadHttp m, MonadIO m, MonadBaseControl IO m) => NodeConfig -> BC.ByteString -> m (Bool)
xRelayTx nodeCnf rawTx = do
    resp <-
        req
            POST
            (https (DT.pack $ xokenListenIP nodeCnf))
            (ReqBodyJson (RelayTx rawTx))
            bsResponse
            (port $ fromEnum $ xokenListenPort nodeCnf)
    case eitherDecodeStrict $ responseBody resp of
        Right (RespRelayTx b) -> undefined
        Left err -> undefined
