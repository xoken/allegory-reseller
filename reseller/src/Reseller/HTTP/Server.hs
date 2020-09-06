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
apiRoutes =
    [ ("/v1/auth", method POST (withReq authClient))
    , ("/v1/user", method POST (withAuthAs "admin" $ withReq addUser))
    , ("/v1/user/:username", method GET (withAuthAs "admin" getUserByUsername))
    , ("/v1/user/:username", method DELETE (withAuthAs "admin" deleteUserByUsername))
    , ("/v1/user/:username", method PUT (withAuthAs "admin" $ withReq updateUserByUsername))
    , ("/v1/user/", method GET (withAuth getCurrentUser))
    , ("/v1/partialsign", method POST (withAuth $ withReq getPartiallySignedAllegoryTx))
    ]
