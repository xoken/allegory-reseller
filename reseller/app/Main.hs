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
import Data.Aeson.Encoding (encodingToLazyByteString, fromEncoding)
import Data.Aeson.Types (parse)
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
import Prelude
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
        nexaEnv = NexaEnv sessionKey
        xknEnv = ResellerEnv lg bitcoinP2PEnv allegoryEnv nexaEnv
        snapConfig =
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
        certFP = tlsCertificatePath nodeCnf
        keyFP = tlsKeyfilePath nodeCnf
        csrFP = tlsCertificateStorePath nodeCnf
        nsk = nameUtxoSecretKey nodeCnf
        fsk = fundUtxoSecretKey nodeCnf
        xpk = NC.xPrivKey nodeCnf
    cfp <- doesFileExist certFP
    kfp <- doesFileExist keyFP
    csfp <- doesDirectoryExist csrFP
    unless (cfp && kfp && csfp) $ P.error "Error: Missing TLS certificate or keyfile"
    runNode (nsk, xpk, fsk) nodeCnf bitcoinP2PEnv [certFP, keyFP, csrFP]

main :: IO ()
main = do
    let pid = "/tmp/reseller.pid.0"
    runDetached (Just pid) (ToFile "reseller.log") initReseller
