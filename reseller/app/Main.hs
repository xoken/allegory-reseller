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
import Crypto.TripleSec as TS
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
import Data.Either
import Data.Function
import Data.Functor.Identity
import qualified Data.HashTable.IO as H
import Data.IORef
import Data.Int
import Data.List
import Data.Map.Strict as M
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
import Reseller.Common
import Reseller.Env
import Reseller.HTTP.Server
import Reseller.HTTP.Types
import qualified Snap as Snap
import StmContainers.Map as SM
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe
import qualified System.Logger as LG
import qualified System.Logger.Class as LGC
import System.Posix.Daemon
import System.Random
import Text.Read (readMaybe)

type HashTable k v = H.BasicHashTable k v

runThreads :: (SecKey, SecKey) -> NodeConfig -> BitcoinP2P -> LG.Logger -> [FilePath] -> IO ()
runThreads (nsk, fsk) nodeConf bitcoinP2PEnv lg certPaths = do
    let addresses =
            (fromJust . addrToString (NC.bitcoinNetwork nodeConf) . pubKeyAddr . derivePubKeyI . wrapSecKey True) <$>
            [nsk, fsk]
    P.putStrLn $ "nUTXO address: " <> (show $ addresses !! 0)
    P.putStrLn $ "Funding address: " <> (show $ addresses !! 1)
    sessionKey <-
        (\k ->
             case k of
                 Left e -> P.error $ "Error: Failed to acquire Nexa session key: " <> (show e)
                 Right k' -> return k') =<<
        (getNexaSessionKey
             (nexaListenIP nodeConf <> ":" <> (show $ nexaListenPort nodeConf))
             (nexaUsername nodeConf)
             (nexaPassword nodeConf))
    let allegoryEnv = AllegoryEnv nsk fsk
        nexaEnv = NexaEnv sessionKey
        xknEnv = ResellerEnv lg bitcoinP2PEnv allegoryEnv nexaEnv
        snapConfig =
            Snap.defaultConfig & Snap.setSSLBind (DTE.encodeUtf8 $ DT.pack $ endPointHTTPSListenIP nodeConf) &
            Snap.setSSLPort (fromEnum $ endPointHTTPSListenPort nodeConf) &
            Snap.setSSLKey (certPaths !! 1) &
            Snap.setSSLCert (head certPaths) &
            Snap.setSSLChainCert False
    Snap.serveSnaplet snapConfig (appInit xknEnv)

runNode :: (SecKey, SecKey) -> NodeConfig -> BitcoinP2P -> [FilePath] -> IO ()
runNode secKeys nodeConf bitcoinP2PEnv certPaths = do
    lg <-
        LG.new
            (LG.setOutput
                 (LG.Path $ T.unpack $ logFileName nodeConf)
                 (LG.setLogLevel (logLevel nodeConf) LG.defSettings))
    runThreads secKeys nodeConf bitcoinP2PEnv lg certPaths

initReseller :: IO ()
initReseller = do
    putStrLn $ "Starting Allegory Reseller..."
    !nodeCnf <- readConfig "reseller-config.yaml"
    udc <- H.new
    let bitcoinP2PEnv = BitcoinP2P nodeCnf udc
        certFP = tlsCertificatePath nodeCnf
        keyFP = tlsKeyfilePath nodeCnf
        csrFP = tlsCertificateStorePath nodeCnf
        fsk = fundUtxoSecretKey nodeCnf
        seed = nameUtxoEncryptedSeed nodeCnf
    cfp <- doesFileExist certFP
    kfp <- doesFileExist keyFP
    csfp <- doesDirectoryExist csrFP
    unless (cfp && kfp && csfp) $ P.error "Error: Missing TLS certificate or keyfile. Quitting."
    putStr "Enter nameUtxo secret key passphrase: " >> hFlush stdout
    passphrase <- getLine
    nsk <- decryptSeed seed passphrase
    let pid = "/tmp/reseller.pid.0"
    runRes <-
        liftIO $
        try $
        runDetached (Just pid) (ToFile "reseller.log") $ runNode (nsk, fsk) nodeCnf bitcoinP2PEnv [certFP, keyFP, csrFP]
    case runRes of
        Left (e :: SomeException) -> do
            putStrLn $ "Encountered fatal exception: " <> show e
            putStrLn $ "Quitting..."
        Right _ -> return ()

decryptSeed :: String -> String -> IO SecKey
decryptSeed encryptedSeed passphrase = do
    let b64Decoded = fromRight (throw SeedDecodeException) $ B64.decode $ C.pack encryptedSeed
    decryptResult <- try $ TS.decryptIO (C.pack passphrase) b64Decoded
    case decryptResult of
        Left (e :: TripleSecException) -> throw PassphraseException
        Right key ->
            return $
            fromMaybe (throw SecKeyException) $
            secKey $ fromMaybe (throw SecKeyException) $ decodeHex $ DT.pack $ C.unpack key

main :: IO ()
main = initReseller
