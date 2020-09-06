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
import Reseller.Service.User
import Reseller.Service.Xoken
import Snap
import qualified System.Logger as LG

authClient :: ReqParams -> Handler App App ()
authClient AuthenticateReq {..} = do
    pretty <- (maybe True (read . DT.unpack . DTE.decodeUtf8)) <$> (getQueryParam "pretty")
    resp <- LE.try $ login (DT.pack username) (BC.pack password)
    case resp of
        Left (e :: SomeException) -> do
            modifyResponse $ setResponseStatus 500 "Internal Server Error"
            writeBS "INTERNAL_SERVER_ERROR"
        Right ar -> writeBS $ BSL.toStrict $ encodeResp pretty $ AuthenticateResp ar
authClient _ = throwBadRequest

addUser :: ReqParams' -> Handler App App ()
addUser AddUser {..} = do
    pretty <- (maybe True (read . DT.unpack . DTE.decodeUtf8)) <$> (getQueryParam "pretty")
    resp <- LE.try $ return $ addNewUser auUsername auFirstName auLastName auEmail auRoles auApiQuota auApiExpiryTime
    case resp of
        Left (e :: SomeException) -> do
            modifyResponse $ setResponseStatus 500 "Internal Server Error"
            writeBS "INTERNAL_SERVER_ERROR"
        Right ar -> do
            ar' <- liftIO $ ar
            case ar' of
                Nothing -> throwBadRequest
                Just aur -> writeBS $ BSL.toStrict $ encodeResp pretty $ RespAddUser aur
addUser _ = throwBadRequest

getCurrentUser :: Handler App App ()
getCurrentUser = do
    sk <- (fmap $ DTE.decodeUtf8) <$> (getParam "sessionKey")
    pretty <- (maybe True (read . DT.unpack . DTE.decodeUtf8)) <$> (getQueryParam "pretty")
    res <- LE.try $ xGetUserBySessionKey (fromJust sk)
    case res of
        Left (e :: SomeException) -> do
            modifyResponse $ setResponseStatus 500 "Internal Server Error"
            writeBS "INTERNAL_SERVER_ERROR"
        Right u@(Just us) -> writeBS $ BSL.toStrict $ encodeResp pretty $ RespUser u
        Right Nothing -> throwNotFound

getUserByUsername :: Handler App App ()
getUserByUsername = do
    uname <- (fmap $ DTE.decodeUtf8) <$> (getParam "username")
    pretty <- (maybe True (read . DT.unpack . DTE.decodeUtf8)) <$> (getQueryParam "pretty")
    res <- LE.try $ xGetUserByUsername (fromJust uname)
    case res of
        Left (e :: SomeException) -> do
            modifyResponse $ setResponseStatus 500 "Internal Server Error"
            writeBS "INTERNAL_SERVER_ERROR"
        Right u@(Just us) -> writeBS $ BSL.toStrict $ encodeResp pretty $ RespUser u
        Right Nothing -> throwNotFound

deleteUserByUsername :: Handler App App ()
deleteUserByUsername = do
    uname <- (fmap $ DTE.decodeUtf8) <$> (getParam "username")
    pretty <- (maybe True (read . DT.unpack . DTE.decodeUtf8)) <$> (getQueryParam "pretty")
    res <- LE.try $ xDeleteUserByUsername (fromJust uname)
    case res of
        Left (e :: SomeException) -> do
            modifyResponse $ setResponseStatus 500 "Internal Server Error"
            writeBS "INTERNAL_SERVER_ERROR"
        Right () -> do
            modifyResponse $ setResponseStatus 200 "Deleted"
            writeBS $ "User deleted"

updateUserByUsername :: UpdateUserByUsername' -> Handler App App ()
updateUserByUsername updates = do
    uname <- (fmap $ DTE.decodeUtf8) <$> (getParam "username")
    pretty <- (maybe True (read . DT.unpack . DTE.decodeUtf8)) <$> (getQueryParam "pretty")
    res <- LE.try $ xUpdateUserByUsername (fromJust uname) updates
    case res of
        Left (e :: SomeException) -> do
            modifyResponse $ setResponseStatus 500 "Internal Server Error"
            writeBS "INTERNAL_SERVER_ERROR"
        Right True -> do
            modifyResponse $ setResponseStatus 200 "Updated"
            writeBS $ "User updated"
        Right False -> throwNotFound

getPartiallySignedAllegoryTx :: ReqParams' -> Handler App App ()
getPartiallySignedAllegoryTx GetPartiallySignedAllegoryTx {..} = do
    pretty <- (maybe True (read . DT.unpack . DTE.decodeUtf8)) <$> (getQueryParam "pretty")
    res <- LE.try $ xGetPartiallySignedAllegoryTx gpsaPaymentInputs gpsaName gpsaOutputOwner gpsaOutputChange
    case res of
        Left (e :: SomeException) -> do
            modifyResponse $ setResponseStatus 500 "Internal Server Error"
            writeBS "INTERNAL_SERVER_ERROR"
        Right ops -> do
            writeBS $ BSL.toStrict $ encodeResp pretty $ RespPartiallySignedAllegoryTx ops
getPartiallySignedAllegoryTx _ = throwBadRequest

--- |
-- Helper functions
withAuth :: Handler App App () -> Handler App App ()
withAuth onSuccess = do
    rq <- getRequest
    env <- gets _env
    let mh = getHeader "Authorization" rq
    let h = parseAuthorizationHeader mh
    case h of
        Just sk -> putRequest $ rqSetParam "sessionKey" [sk] rq
        Nothing -> return ()
    uok <- undefined -- liftIO $ testAuthHeader env h Nothing
    modifyResponse (setContentType "application/json")
    if uok
        then onSuccess
        else case h of
                 Nothing -> throwChallenge
                 Just _ -> throwDenied

withAuthAs :: DT.Text -> Handler App App () -> Handler App App ()
withAuthAs role onSuccess = do
    rq <- getRequest
    env <- gets _env
    let mh = getHeader "Authorization" rq
    let h = parseAuthorizationHeader mh
    case h of
        Just sk -> putRequest $ rqSetParam "sessionKey" [sk] rq
        Nothing -> return ()
    uok <- undefined -- liftIO $ testAuthHeader env h $ Just role
    modifyResponse (setContentType "application/json")
    if uok
        then onSuccess
        else case h of
                 Nothing -> throwChallenge
                 Just _ -> throwDenied

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

parseAuthorizationHeader :: Maybe B.ByteString -> Maybe B.ByteString
parseAuthorizationHeader bs =
    case bs of
        Nothing -> Nothing
        Just x ->
            case (S.split ' ' x) of
                ("Bearer":y) ->
                    if S.length (S.intercalate "" y) > 0
                        then Just $ S.intercalate "" y
                        else Nothing
                _ -> Nothing

testAuthHeader :: ResellerEnv -> Maybe B.ByteString -> Maybe String -> IO Bool
testAuthHeader _ Nothing _ = pure False
testAuthHeader env (Just sessionKey) role = do
    let lg = loggerEnv env
        bp2pEnv = bitcoinP2PEnv env
        sKey = DT.pack $ S.unpack sessionKey
    userData <- liftIO $ H.lookup (userDataCache bp2pEnv) sKey
    case userData of
        Just (name, quota, used, exp, roles) -> do
            curtm <- liftIO $ getCurrentTime
            if exp > curtm && quota > used
                then do
                    if (used + 1) `mod` 100 == 0
                            --let str = " UPDATE xoken.user_permission SET api_used = ? WHERE username = ? "
                                --qstr = str :: Q.QueryString Q.W (Int32, DT.Text) ()
                                --p = getSimpleQueryParam (used + 1, name)
                        then do
                            res <- undefined -- liftIO $ try $ write conn (Q.RqQuery $ Q.Query qstr p)
                            case res of
                                Left (SomeException e) -> do
                                    LG.err lg $ LG.msg $ "Error: UPDATE'ing into 'user_permission': " ++ show e
                                    throw e
                                Right _ -> return ()
                        else return ()
                    liftIO $ H.insert (userDataCache bp2pEnv) sKey (name, quota, used + 1, exp, roles)
                    case role of
                        Nothing -> return True
                        Just rl -> return $ rl `elem` (DT.unpack <$> roles)
                else do
                    liftIO $ H.delete (userDataCache bp2pEnv) sKey
                    return False
        Nothing
            --let str =
                    --" SELECT username, api_quota, api_used, session_key_expiry_time, permissions FROM xoken.user_permission WHERE session_key = ? ALLOW FILTERING "
                --qstr = str :: Q.QueryString Q.R (Identity DT.Text) (DT.Text, Int32, Int32, UTCTime, Set DT.Text)
                --p = getSimpleQueryParam $ Identity $ sKey
         -> do
            res <- undefined -- liftIO $ try $ query conn (Q.RqQuery $ Q.Query qstr p)
            case res of
                Left (SomeException e) -> do
                    LG.err lg $ LG.msg $ "Error: SELECT'ing from 'user_permission': " ++ show e
                    throw e
                Right op -> do
                    if length op == 0
                        then return False
                        else do
                            case op !! 0 of
                                User {..} -> do
                                    curtm <- liftIO $ getCurrentTime
                                    if uApiExpiryTime > curtm && uApiQuota > uApiUsed
                                        then do
                                            liftIO $
                                                H.insert
                                                    (userDataCache bp2pEnv)
                                                    sKey
                                                    ( (DT.pack uUsername)
                                                    , (toEnum uApiQuota)
                                                    , (toEnum $ fromEnum uApiUsed + 1)
                                                    , uApiExpiryTime
                                                    , (DT.pack <$> uRoles))
                                            case role of
                                                Nothing -> return True
                                                Just rl -> return $ rl `elem` uRoles
                                        else return False

throwChallenge :: Handler App App ()
throwChallenge = do
    modifyResponse $
        (setResponseStatus 401 "Unauthorized") . (setHeader "WWW-Authenticate" "Basic realm=my-authentication")
    writeBS ""

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
