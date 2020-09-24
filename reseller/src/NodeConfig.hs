{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NodeConfig
    ( module NodeConfig
    ) where

import Control.Exception
import Control.Monad (guard)
import Crypto.Secp256k1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Char8 as C
import Data.Int
import Data.Maybe
import Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Word
import Data.Yaml
import GHC.Generics
import Network.Socket
import Network.Xoken.Constants
import System.Logger

data NodeConfig =
    NodeConfig
        { bitcoinNetwork :: !Network
        , logLevel :: !Level
        , logFileName :: !T.Text
        , endPointHTTPSListenIP :: !String
        , endPointHTTPSListenPort :: !PortNumber
        , encryptedSeed :: !ByteString
        , tlsCertificatePath :: !FilePath
        , tlsKeyfilePath :: FilePath
        , tlsCertificateStorePath :: !FilePath
        , defaultSathosi :: !Int
        , feeSatsCreate :: !Int
        , feeSatsTransfer :: !Int
        , nameUtxoSatoshis :: !Int
        , xokenListenIP :: !String
        , xokenListenPort :: !PortNumber
        , nexaUsername :: !String
        , nexaPassword :: !String
        }
    deriving (Show, Generic)

instance FromJSON ByteString where
    parseJSON = withText "ByteString" $ \t -> pure (E.encodeUtf8 t)

instance FromJSON PortNumber where
    parseJSON v = fromInteger <$> parseJSON v

instance FromJSON SecKey where
    parseJSON v = fromJust <$> secKey <$> (parseJSON v :: Parser ByteString)

instance FromJSON Network where
    parseJSON v = fromJust <$> netByName <$> parseJSON v

instance FromJSON Level where
    parseJSON v = read <$> parseJSON v

instance FromJSON NodeConfig

readConfig :: FilePath -> IO NodeConfig
readConfig path = do
    config <- decodeFileEither path :: IO (Either ParseException NodeConfig)
    case config of
        Left e -> print e *> throw e
        Right con -> return con
-- | Decode string of human-readable hex characters.
--decodeHex :: Text -> Maybe ByteString
--decodeHex text =
--    let (x, b) = B16.decode (E.encodeUtf8 text)
--     in guard (b == BS.empty) >> return x
