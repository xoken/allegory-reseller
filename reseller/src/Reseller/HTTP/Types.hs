{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Reseller.HTTP.Types where

import Codec.Serialise
import Control.Applicative
import qualified Control.Exception as CE
import Control.Lens (makeLenses)
import qualified Control.Monad.Catch as MC
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Control
import Crypto.Secp256k1
import Data.Aeson
import Data.ByteString
import Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Char as Char
import qualified Data.HashTable.IO as H
import Data.Int
import qualified Data.Map.Strict as M
import Data.Text
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Data.Word
import GHC.Generics
import Network.HTTP.Req
import Prelude
import Reseller.Env
import Snap

data App =
    App
        { _env :: ResellerEnv
        }

instance HasBitcoinP2P (Handler App App) where
    getBitcoinP2P = bitcoinP2PEnv <$> gets _env

instance HasAllegoryEnv (Handler App App) where
    getAllegory = allegoryEnv <$> gets _env

instance HasLogger (Handler App App) where
    getLogger = loggerEnv <$> gets _env

instance MC.MonadThrow (Handler App App) where
    throwM = liftIO . CE.throwIO

instance MonadUnliftIO (Handler App App) --where

instance MonadHttp (Handler App App) where
    handleHttpException = MC.throwM

-- Request & Response Types
--
data ReqParams
    = AuthenticateReq
          { username :: String
          , password :: String
          , prettyPrint :: Bool
          }
    | GeneralReq
          { sessionKey :: String
          , prettyPrint :: Bool
          , methodParams :: Maybe ReqParams'
          }
    deriving (Show, Generic, Eq, Serialise)

instance FromJSON ReqParams where
    parseJSON (Object o) =
        (AuthenticateReq <$> o .: "username" <*> o .: "password" <*> o .:? "prettyPrint" .!= True) <|>
        (GeneralReq <$> o .: "sessionKey" <*> o .:? "prettyPrint" .!= True <*> o .:? "methodParams")

data ReqParams'
    = AddUser
          { auUsername :: String
          , auApiExpiryTime :: Maybe UTCTime
          , auApiQuota :: Maybe Int32
          , auFirstName :: String
          , auLastName :: String
          , auEmail :: String
          , auRoles :: Maybe [String]
          }
    | UserByUsername
          { uUsername :: String
          }
    | UpdateUserByUsername
          { uuUsername :: String
          , uuUpdateData :: UpdateUserByUsername'
          }
    | GetPartiallySignedAllegoryTx
          { gpsaPaymentInputs :: [(OutPoint', Int)]
          , gpsaName :: ([Int], Bool) -- name & isProducer
          , gpsaOutputOwner :: String
          , gpsaOutputChange :: String
          }
    | RelayTx
          { rTx :: ByteString
          }
    deriving (Generic, Show, Eq, Serialise, ToJSON)

instance FromJSON ReqParams' where
    parseJSON (Object o) =
        (AddUser <$> o .: "username" <*> o .:? "apiExpiryTime" <*> o .:? "apiQuota" <*> o .: "firstName" <*>
         o .: "lastName" <*>
         o .: "email" <*>
         o .:? "roles") <|>
        (UserByUsername <$> o .: "username") <|>
        (UpdateUserByUsername <$> o .: "username" <*> o .: "updateData") <|>
        (GetPartiallySignedAllegoryTx <$> o .: "paymentInputs" <*> o .: "name" <*> o .: "outputOwner" <*>
         o .: "outputChange") <|>
        (RelayTx . B64.decodeLenient . T.encodeUtf8 <$> o .: "rawTx")

data ResponseBody
    = AuthenticateResp
          { auth :: AuthResp
          }
    | RespAddUser
          { addUser :: AddUserResp
          }
    | RespUser
          { user :: Maybe User
          }
    | RespPartiallySignedAllegoryTx
          { psaTx :: ByteString
          }
    | RespRelayTx
          { rrTx :: Bool
          }
    deriving (Generic, Show, Eq, Serialise)

instance ToJSON ResponseBody where
    toJSON (AuthenticateResp a) = object ["auth" .= a]
    toJSON (RespAddUser usr) = object ["user" .= usr]
    toJSON (RespUser u) = object ["user" .= u]
    toJSON (RespPartiallySignedAllegoryTx ps) = object ["psaTx" .= (T.decodeUtf8 . B64.encode $ ps)]
    toJSON (RespRelayTx rrTx) = object ["txBroadcast" .= rrTx]

instance FromJSON ResponseBody where
    parseJSON (Object o) =
        (AuthenticateResp <$> o .: "auth") <|> (RespAddUser <$> o .: "user") <|> (RespUser <$> o .: "user") <|>
        (RespPartiallySignedAllegoryTx <$> o .: "psaTx") <|>
        (RespRelayTx <$> o .: "txBroadcast")

data UpdateUserByUsername' =
    UpdateUserByUsername'
        { uuPassword :: Maybe String
        , uuFirstName :: Maybe String
        , uuLastName :: Maybe String
        , uuEmail :: Maybe String
        , uuApiQuota :: Maybe Int32
        , uuRoles :: Maybe [String]
        , uuApiExpiryTime :: Maybe UTCTime
        }
    deriving (Generic, Show, Eq, Serialise, ToJSON)

instance FromJSON UpdateUserByUsername' where
    parseJSON (Object o) =
        (UpdateUserByUsername' <$> o .:? "password" <*> o .:? "firstName" <*> o .:? "lastName" <*> o .:? "email" <*>
         o .:? "apiQuota" <*>
         o .:? "roles" <*>
         o .:? "apiExpiryTime")

data AuthResp =
    AuthResp
        { sessionKey :: Maybe String
        , callsUsed :: Int
        , callsRemaining :: Int
        }
    deriving (Generic, Show, Eq, Serialise, ToJSON, FromJSON)

data AddUserResp =
    AddUserResp
        { aurUser :: User
        , aurPassword :: String
        }
    deriving (Generic, Show, Eq, Serialise, FromJSON)

instance ToJSON AddUserResp where
    toJSON (AddUserResp (User uname _ fname lname email roles apiQuota _ apiExpTime _ _) pwd) =
        object
            [ "username" .= uname
            , "password" .= pwd
            , "firstName" .= fname
            , "lastName" .= lname
            , "email" .= email
            , "roles" .= roles
            , "apiQuota" .= apiQuota
            , "apiExpiryTime" .= apiExpTime
            ]

data User =
    User
        { uUsername :: String
        , uHashedPassword :: String
        , uFirstName :: String
        , uLastName :: String
        , uEmail :: String
        , uRoles :: [String]
        , uApiQuota :: Int
        , uApiUsed :: Int
        , uApiExpiryTime :: UTCTime
        , uSessionKey :: String
        , uSessionKeyExpiry :: UTCTime
        }
    deriving (Generic, Show, Eq, Serialise)

instance ToJSON User where
    toJSON =
        genericToJSON
            (defaultOptions
                 { fieldLabelModifier =
                       \x ->
                           let (h:t) = Prelude.drop 1 x
                            in Char.toLower h : t
                 })

instance FromJSON User where
    parseJSON =
        genericParseJSON
            (defaultOptions
                 { fieldLabelModifier =
                       \x ->
                           let (h:t) = x
                            in "u" <> (Char.toUpper h : t)
                 })

data OutPoint' =
    OutPoint'
        { opTxHash :: String
        , opIndex :: Int32
        }
    deriving (Show, Generic, Eq, Serialise, FromJSON, ToJSON)

makeLenses ''App
