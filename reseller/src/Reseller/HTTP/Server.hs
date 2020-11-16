{-# LANGUAGE OverloadedStrings #-}

module Reseller.HTTP.Server where

import qualified Data.ByteString as B
import Reseller.Env
import Reseller.HTTP.Handler
import Reseller.HTTP.Types (App(..))
import Snap

appInit :: ResellerEnv -> SnapletInit App App
appInit env =
    makeSnaplet "v1" "API's" Nothing $ do
        addRoutes apiRoutes
        return $ App env

apiRoutes :: [(B.ByteString, Handler App App ())]
apiRoutes = [("/v1/partialsign", method POST (withReq getPartiallySignedAllegoryTx))]
