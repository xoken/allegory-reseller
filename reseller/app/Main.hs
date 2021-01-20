{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import Control.Arrow
import Control.Concurrent (threadDelay)
import Control.Concurrent
import qualified Control.Concurrent.Async as A (async, uninterruptibleCancel)
import Control.Concurrent.Async.Lifted as LA (async, race, wait, withAsync)
import Control.Concurrent.Event as EV
import Control.Concurrent.MSem as MS
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Concurrent.STM.TQueue as TB
import Control.Concurrent.STM.TVar
import Control.Exception (throw)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Loops
import Control.Monad.Reader
import qualified Control.Monad.STM as CMS (atomically)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import Crypto.KDF.Scrypt (Parameters(..), generate)
import Crypto.Secp256k1
import Data.Aeson as A
import Data.Aeson.Types (parse)
import Data.Aeson.Encoding (encodingToLazyByteString, fromEncoding)
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Base64 as B64
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Char
import Data.Default
import Data.Default
import Data.Function
import Data.Functor.Identity
import qualified Data.HashTable.IO as H
import Data.IORef
import Data.Int
import Data.List
import Data.Map.Strict as M
import Data.Maybe
import Data.Maybe
import Data.Pool
import Data.Serialize as Serialize
import Data.Serialize as S
import Data.String.Conv
import Data.String.Conversions
import qualified Data.Text as DT
import qualified Data.Text as T
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Lazy as TL
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Typeable
import Data.Version
import Data.Word (Word32)
import Data.Word
import qualified LevelDB as DL
import Network.Simple.TCP
import Network.Socket
import Network.Xoken.Address
import Network.Xoken.Keys
import Network.Xoken.Util
import Nexa
import Nexa.Auth
import NodeConfig as NC
import Options.Applicative
import Paths_reseller as P
import Prelude as P
import Reseller.Env
import Reseller.HTTP.Server
import Reseller.HTTP.Types
import Reseller.Service.User
import qualified Snap as Snap
import StmContainers.Map as SM
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.Exit
import System.FilePath
import System.IO.Unsafe
import qualified System.Logger as LG
import qualified System.Logger.Class as LGC
import System.Posix.Daemon
import System.Random
import Text.Read (readMaybe)
import Prelude

data ConfigException
    = ConfigParseException
    | RandomSecretKeyException
    deriving (Eq, Ord, Show)

instance Exception ConfigException

type HashTable k v = H.BasicHashTable k v

runThreads :: (SecKey, String, SecKey) -> NodeConfig -> BitcoinP2P -> LG.Logger -> [FilePath] -> IO ()
runThreads (nsk, xPrivKey, fsk) nodeConf bitcoinP2PEnv lg certPaths = do
    putStrLn $ "Acquiring Nexa session key for user " <> (nexaUsername nodeConf) <> "..."
    sessionKey <-
        (\k ->
             case k of
                 Left e -> P.error $ "Error: Failed to acquire Nexa session key: " <> (show e)
                 Right k' -> return k') =<<
        (getNexaSessionKey
             (nexaListenIP nodeConf <> ":" <> (show $ nexaListenPort nodeConf))
             (nexaUsername nodeConf)
             (nexaPassword nodeConf))
    putStrLn $ "Acquired Nexa session key: " <> (show sessionKey)
    let allegoryEnv = AllegoryEnv nsk xPrivKey fsk
    let nexaEnv = NexaEnv sessionKey
    let xknEnv = ResellerEnv lg bitcoinP2PEnv allegoryEnv nexaEnv
    -- start HTTP endpoint
    let snapConfig =
            Snap.defaultConfig & Snap.setSSLBind (DTE.encodeUtf8 $ DT.pack $ endPointHTTPSListenIP nodeConf) &
            Snap.setSSLPort (fromEnum $ endPointHTTPSListenPort nodeConf) &
            Snap.setSSLKey (certPaths !! 1) &
            Snap.setSSLCert (head certPaths) &
            Snap.setSSLChainCert False
    Snap.serveSnaplet snapConfig (appInit xknEnv)

runNode :: (SecKey, String, SecKey) -> NodeConfig -> BitcoinP2P -> [FilePath] -> IO ()
runNode secKeys nodeConf bitcoinP2PEnv certPaths = do
    lg <-
        LG.new
            (LG.setOutput
                 (LG.Path $ T.unpack $ logFileName nodeConf)
                 (LG.setLogLevel (logLevel nodeConf) LG.defSettings))
    runThreads secKeys nodeConf bitcoinP2PEnv lg certPaths

initReseller :: IO ()
initReseller = do
    putStrLn $ "--------------------------"
    putStrLn $ "Starting Allegory Reseller"
    putStrLn $ "--------------------------"
    !nodeCnf <- readConfig "reseller-config.yaml"
    udc <- H.new
    let bitcoinP2PEnv = BitcoinP2P nodeCnf udc
    let certFP = tlsCertificatePath nodeCnf
        keyFP = tlsKeyfilePath nodeCnf
        csrFP = tlsCertificateStorePath nodeCnf
        host = nexaListenIP nodeCnf <> ":" <> (show $ nexaListenPort nodeCnf)
        nsk = nameUtxoSecretKey nodeCnf
        xPrivKey' = NC.xPrivKey nodeCnf
        fsk = fundUtxoSecretKey nodeCnf
        user = nexaUsername nodeCnf
        pass = nexaPassword nodeCnf
    putStrLn $ "shub"
    -- let xPriv' = A.String $ DT.pack xPrivKey'
    -- let net = bitcoinNetwork nodeCnf
    -- xPriv <- case parse Prelude.id . xPrvFromJSON net $ xPriv' of
    --             A.Success k -> return k
    --             A.Error e -> undefined
    -- let testAddr = fst $ derivePathAddr (deriveXPubKey x) (Deriv :/ 44 :/ 1 :/ 1 :/0 :: SoftPath) 1
    -- let nameAddr = fst $ derivePathAddr x (Deriv :/ 44 :/ 1 :/ 1 :/0 :: SoftPath) 1
    -- let deriveXpub = derivePubPath (Deriv :/ 44 :/ 1 :/ 1 :/0 :: SoftPath) (deriveXPubKey xPriv)
    -- let deriveXpriv = derivePath (Deriv :/ 44 :/ 1 :/ 1 :/0 :: SoftPath) xPriv
    -- let nUtxoAddr = deriveAddr pubPath (fromIntegral 1)
    -- let nUtxoSecKey = deriveAddr pubPath (fromIntegral 1)
    -- putStrLn $ show $ addrToString net (fst nUtxoAddr)
    cfp <- doesFileExist certFP
    kfp <- doesFileExist keyFP
    csfp <- doesDirectoryExist csrFP
    unless (cfp && kfp && csfp) $ P.error "Error: Missing TLS certificate or keyfile"
    runNode (nsk, xPrivKey', fsk) nodeCnf bitcoinP2PEnv [certFP, keyFP, csrFP]

defaultAdminUser :: IO ()
defaultAdminUser = do
    op <- DL.getValue (DTE.encodeUtf8 "admin")
    putStrLn $ "Starting Reseller"
    case op of
        Just _ -> return ()
        Nothing -> do
            tm <- liftIO $ getCurrentTime
            usr <-
                addNewUser
                    "admin"
                    "default"
                    "user"
                    ""
                    (Just ["admin"])
                    (Just 100000000)
                    (Just (addUTCTime (nominalDay * 365) tm))
            putStrLn $ "******************************************************************* "
            putStrLn $ "  Creating default Admin user!"
            putStrLn $ "  Please note down admin password NOW, will not be shown again."
            putStrLn $ "  Password : " ++ (aurPassword $ fromJust usr)
            putStrLn $ "******************************************************************* "

relaunch :: B.ByteString -> IO ()
relaunch password =
    forever $ do
        let pid = "/tmp/nexa.pid.1"
        running <- isRunning pid
        if running
            then threadDelay (30 * 1000000)
            else do
                runDetached (Just pid) (ToFile "nexa.log") initReseller
                threadDelay (5000000)

main :: IO ()
main = do
    let pid = "/tmp/nexa.pid.0"
    initReseller
    --runDetached (Just pid) (ToFile "nexa.log") relaunch

saltSize = 32

paramN = 16 :: Word64

paramR = 8

paramP = 1

paramKeyLen = 32

--Scrypt KDF
deriveKey :: B.ByteString -> B.ByteString
deriveKey password = generate params password ("" :: B.ByteString)
  where
    params = Parameters {n = paramN, r = paramR, p = paramP, outputLength = paramKeyLen}
