{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Reseller.Service.User where

import Codec.Serialise
import Conduit hiding (runResourceT)
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (AsyncCancelled, mapConcurrently, mapConcurrently_, race_)
import qualified Control.Concurrent.Async.Lifted as LA (async, concurrently, mapConcurrently, wait)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Control.Error.Util as Extra
import Control.Exception
import qualified Control.Exception.Lifted as LE (try)
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16 (decode, encode)
import Data.ByteString.Base64 as B64
import Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.UTF8 as BSU (toString)
import Data.Char
import Data.Default
import qualified Data.HashTable.IO as H
import Data.Hashable
import Data.IORef
import Data.Int
import Data.List
import qualified Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.Pool
import qualified Data.Serialize as S
import Data.Serialize
import qualified Data.Serialize as DS (decode, encode)
import qualified Data.Set as S
import Data.String (IsString, fromString)
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Encoding as E
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word
import Data.Yaml
import qualified LevelDB as DL
import qualified NodeConfig as NC
import Numeric (showHex)
import Reseller.Common
import Reseller.Env
import Reseller.HTTP.Types
import System.Logger as LG
import System.Logger.Message
import System.Random
import Text.Read
import Xoken

xGetUserByUsername :: (HasResellerEnv env m, MonadIO m, MonadUnliftIO m) => DT.Text -> m (Maybe User)
xGetUserByUsername name = do
    lg <- getLogger
    bp2pEnv <- getBitcoinP2P
    res <- DL.getValue (DTE.encodeUtf8 name)
    case res of
        Just ibs -> do
            case eitherDecodeStrict ibs of
                Right (iop :: [User]) -> do
                    if length iop == 0
                        then return Nothing
                        else do
                            let User {..} = iop !! 0
                            userData <- liftIO $ H.lookup (userDataCache bp2pEnv) (DT.pack uSessionKey)
                            case userData of
                                Just (_, _, used, _, _) ->
                                    return $
                                    Just $
                                    User
                                        uUsername
                                        uHashedPassword
                                        uFirstName
                                        uLastName
                                        uEmail
                                        uRoles
                                        (fromIntegral uApiQuota)
                                        (fromIntegral uApiUsed)
                                        uApiExpiryTime
                                        (maskAfter 10 uSessionKey)
                                        uSessionKeyExpiry
                                Nothing ->
                                    return $
                                    Just $
                                    User
                                        uUsername
                                        uHashedPassword
                                        uFirstName
                                        uLastName
                                        uEmail
                                        uRoles
                                        (fromIntegral uApiQuota)
                                        (fromIntegral uApiUsed)
                                        uApiExpiryTime
                                        (maskAfter 10 uSessionKey)
                                        uSessionKeyExpiry
        Nothing -> do
            err lg $ LG.msg $ ("Error: xGetUserByUsername: No user found" :: DT.Text)
            throw KeyValueDBLookupException

xDeleteUserByUsername :: (HasResellerEnv env m, MonadIO m, MonadUnliftIO m) => DT.Text -> m ()
xDeleteUserByUsername name = do
    lg <- getLogger
    bp2pEnv <- getBitcoinP2P
    res <- LE.try $ DL.getValue (DTE.encodeUtf8 name)
    case res of
        Right (Just _) -> do
            cacheList <- liftIO $ (H.toList $ userDataCache bp2pEnv)
            liftIO $
                mapM_
                    (\(k, (n, _, _, _, _)) ->
                         if n == name
                             then H.delete (userDataCache bp2pEnv) k
                             else return ())
                    cacheList
        --Right Nothing -> debug lg $ LG.msg $ "Debug: no user found to delete"
        Left (e :: SomeException) -> do
            err lg $ LG.msg $ "Error: xDeleteUserByUsername: " ++ show e
            throw e

xUpdateUserByUsername ::
       (HasResellerEnv env m, MonadIO m, MonadUnliftIO m) => DT.Text -> UpdateUserByUsername' -> m Bool
xUpdateUserByUsername name (UpdateUserByUsername' {..}) = do
    lg <- getLogger
    bp2pEnv <- getBitcoinP2P
    res <- LE.try $ xGetUserByUsername name
    case res of
        Right (Just (User {..})) -> do
            let usr =
                    User
                        uUsername
                        (fromMaybe
                             (uHashedPassword)
                             ((DT.unpack . encodeHex . S.encode . sha256 . BC.pack) <$> uuPassword))
                        (fromMaybe uFirstName uuFirstName)
                        (fromMaybe uLastName uuLastName)
                        (fromMaybe uEmail uuEmail)
                        (fromMaybe uRoles uuRoles)
                        (maybe uApiQuota fromIntegral uuApiQuota)
                        uApiUsed
                        (fromMaybe uApiExpiryTime uuApiExpiryTime)
                        uSessionKey
                        uSessionKeyExpiry
            res' <- LE.try $ DL.putValue (DTE.encodeUtf8 name) (encodeStrict usr)
            case res' of
                Right _ -> do
                    if isJust uuPassword
                        then do
                            tm <- liftIO $ getCurrentTime
                            newSessionKey <- liftIO $ generateSessionKey
                            let str' =
                                    "UPDATE xoken.user_permission SET session_key=?, session_key_expiry_time=? WHERE username=?"
                                skTime = (addUTCTime (nominalDay * 30) tm)
                                usr' = usr {uSessionKey = DT.unpack newSessionKey, uSessionKeyExpiry = skTime}
                            res'' <- LE.try $ DL.putValue (DTE.encodeUtf8 name) (encodeStrict usr')
                            case res'' of
                                Right _ -> do
                                    cacheList <- liftIO $ (H.toList $ userDataCache bp2pEnv)
                                    liftIO $
                                        mapM_
                                            (\(k, (n, q, u, e, r)) ->
                                                 if n == name
                                                     then do
                                                         H.delete (userDataCache bp2pEnv) k
                                                         liftIO $
                                                             H.insert
                                                                 (userDataCache bp2pEnv)
                                                                 (newSessionKey)
                                                                 ( n
                                                                 , fromMaybe (fromIntegral uApiQuota) uuApiQuota
                                                                 , u
                                                                 , skTime
                                                                 , r)
                                                     else return ())
                                            cacheList
                                    return True
                                Left (e :: SomeException) -> do
                                    err lg $ LG.msg $ "Error: xUpdateUserByUsername (updating sessionKey): " ++ show e
                                    throw e
                        else do
                            liftIO $
                                H.mutate
                                    (userDataCache bp2pEnv)
                                    (DT.pack uSessionKey)
                                    (\v ->
                                         ( (\(n, _, u, e, r) ->
                                                (n, fromMaybe (fromIntegral uApiQuota) uuApiQuota, u, e, r)) <$>
                                           v
                                         , True))
                Left (e :: SomeException) -> do
                    err lg $ LG.msg $ "Error: xUpdateUserByUsername (updating data): " ++ show e
                    throw e
        Right Nothing -> return False
        Left (e :: SomeException) -> do
            err lg $ LG.msg $ "Error: xUpdateUserByUsername: " ++ show e
            throw e

xGetUserBySessionKey :: (HasResellerEnv env m, MonadIO m, MonadUnliftIO m) => DT.Text -> m (Maybe User)
xGetUserBySessionKey skey = do
    lg <- getLogger
    bp2pEnv <- getBitcoinP2P
    res <- LE.try $ DL.getValue (DTE.encodeUtf8 skey)
    case res of
        Right (Just iops) -> do
            case eitherDecodeStrict iops of
                Right User {..} -> do
                    userData <- liftIO $ H.lookup (userDataCache bp2pEnv) (DT.pack uSessionKey)
                    case userData of
                        Just (_, _, used, _, _) ->
                            return $
                            Just $
                            User
                                uUsername
                                ""
                                uFirstName
                                uLastName
                                uEmail
                                uRoles
                                (fromIntegral uApiQuota)
                                (fromIntegral uApiUsed)
                                uApiExpiryTime
                                (maskAfter 10 uSessionKey)
                                uSessionKeyExpiry
                        Nothing ->
                            return $
                            Just $
                            User
                                uUsername
                                ""
                                uFirstName
                                uLastName
                                uEmail
                                uRoles
                                (fromIntegral uApiQuota)
                                (fromIntegral uApiUsed)
                                uApiExpiryTime
                                (maskAfter 10 uSessionKey)
                                uSessionKeyExpiry
                Left e -> do
                    err lg $ LG.msg $ "Error: xGetUserBySessionKey: " ++ e
                    throw KeyValueDBLookupException
        Left (e :: SomeException) -> do
            err lg $ LG.msg $ "Error: xGetUserBySessionKey: " ++ show e
            throw KeyValueDBLookupException

login :: (MonadIO m, HasResellerEnv env m, MonadUnliftIO m) => DT.Text -> BC.ByteString -> m AuthResp
login user pass = do
    lg <- getLogger
    bp2pEnv <- getBitcoinP2P
    let hashedPasswd = encodeHex ((S.encode $ sha256 pass))
    res <- LE.try $ DL.getValue (DTE.encodeUtf8 user)
    case res of
        Right (Just iops) -> do
            case eitherDecodeStrict iops of
                Right u@User {..} -> do
                    if (uHashedPassword /= DT.unpack hashedPasswd)
                        then return $ AuthResp Nothing 0 0
                        else do
                            tm <- liftIO $ getCurrentTime
                            newSessionKey <- liftIO $ generateSessionKey
                            res1 <-
                                LE.try $
                                DL.putValue
                                    (DTE.encodeUtf8 user)
                                    (encodeStrict $
                                     User
                                         uUsername
                                         uHashedPassword
                                         uFirstName
                                         uLastName
                                         uEmail
                                         uRoles
                                         uApiQuota
                                         uApiUsed
                                         uApiExpiryTime
                                         (DT.unpack newSessionKey)
                                         tm)
                            case res1 of
                                Right _ -> do
                                    userData <- liftIO $ H.lookup (userDataCache bp2pEnv) (DT.pack uSessionKey)
                                    case userData of
                                        Just (n, q, u, e, r) -> do
                                            liftIO $ H.delete (userDataCache bp2pEnv) (DT.pack uSessionKey)
                                            liftIO $
                                                H.insert
                                                    (userDataCache bp2pEnv)
                                                    (newSessionKey)
                                                    (n, q, u, (addUTCTime (nominalDay * 30) tm), r)
                                            return $
                                                AuthResp
                                                    (Just $ DT.unpack newSessionKey)
                                                    (fromIntegral u)
                                                    (fromIntegral $ q - u)
                                        Nothing -> do
                                            liftIO $
                                                H.insert
                                                    (userDataCache bp2pEnv)
                                                    (newSessionKey)
                                                    ( (DT.pack uUsername)
                                                    , (fromIntegral uApiQuota)
                                                    , (fromIntegral uApiUsed)
                                                    , (addUTCTime (nominalDay * 30) tm)
                                                    , (DT.pack <$> uRoles))
                                            return $
                                                AuthResp
                                                    (Just $ DT.unpack newSessionKey)
                                                    (fromIntegral uApiUsed)
                                                    (fromIntegral $ uApiQuota - uApiUsed)
                                Left (SomeException e) -> do
                                    err lg $ LG.msg $ "Error: UPDATE'ing into 'user_permission': " ++ show e
                                    throw e
        Left (SomeException e) -> do
            err lg $ LG.msg $ "Error: SELECT'ing from 'user_permission': " ++ show e
            throw e

addNewUser ::
       String -> String -> String -> String -> Maybe [String] -> Maybe Int32 -> Maybe UTCTime -> IO (Maybe AddUserResp)
addNewUser uname fname lname email roles api_quota api_expiry_time = do
    opb <- DL.getValue (DTE.encodeUtf8 $ DT.pack uname)
    case opb of
        Nothing -> do
            tm <- liftIO $ getCurrentTime
            g <- liftIO $ newStdGen
            let seed = show $ fst (random g :: (Word64, StdGen))
                passwd = B.init $ B.init $ B64.encode $ BC.pack $ seed
                hashedPasswd = encodeHex ((S.encode $ sha256 passwd))
                tempSessionKey = encodeHex ((S.encode $ sha256 $ B.reverse passwd))
                par =
                    User
                        uname
                        (DT.unpack hashedPasswd)
                        fname
                        lname
                        email
                        (fromMaybe ["read"] roles)
                        (maybe 10000 fromEnum api_quota)
                        0
                        (fromMaybe (addUTCTime (nominalDay * 365) tm) api_expiry_time)
                        (DT.unpack tempSessionKey)
                        (addUTCTime (nominalDay * 30) tm)
            res1 <- LE.try $ DL.putValue (DTE.encodeUtf8 $ DT.pack uname) (encodeStrict par)
            case res1 of
                Right _ -> do
                    putStrLn $ "Added user: " ++ uname
                    return $
                        Just $
                        AddUserResp
                            (User
                                 uname
                                 ""
                                 fname
                                 lname
                                 email
                                 (fromMaybe ["read"] roles)
                                 (fromIntegral $ fromMaybe 10000 api_quota)
                                 0
                                 (fromMaybe (addUTCTime (nominalDay * 365) tm) api_expiry_time)
                                 (maskAfter 10 $ DT.unpack tempSessionKey)
                                 (addUTCTime (nominalDay * 30) tm))
                            (BC.unpack passwd)
                Left (e :: SomeException) -> do
                    putStrLn $ "Error: INSERTing into 'user_permission': " ++ show e
                    throw e
        Just _ -> return Nothing
