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
import Network.HTTP.Req
import Prelude
import Reseller.HTTP.Types

xGetPartiallySignedAllegoryTx ::
       (MonadIO m, MonadHttp m, MonadBaseControl IO m)
    => [(OutPoint', Int)]
    -> ([Int], Bool)
    -> (String)
    -> (String)
    -> m (BC.ByteString)
xGetPartiallySignedAllegoryTx payips np owner change = do
    resp <-
        req
            POST
            (https "000.00.00.000")
            (ReqBodyJson (GetPartiallySignedAllegoryTx payips np owner change))
            bsResponse
            mempty
    case eitherDecodeStrict $ responseBody resp of
        Right (RespPartiallySignedAllegoryTx pis) -> undefined
        Left err -> undefined
