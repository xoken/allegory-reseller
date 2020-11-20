{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Nexa.Types where

import Codec.Serialise
import Control.Exception
import Data.Aeson
import Data.ByteString as BS
import Data.ByteString.Base64 as B64
import Data.ByteString.Char8 as B8 (pack)
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics

data NexaException
    = AuthException
    | ResponseParseException
    | ServerException
    deriving (Show)

instance Exception NexaException

data NexaRequest
    = AuthenticateRequest
          { username :: String
          , password :: String
          }
    | NameOutpointRequest
          { name :: [Int]
          , isProducer :: Bool
          }
    | GetUTXOsByAddressRequest
          { address :: String
          , pageSize :: Int
          }
    | RelayTxRequest
          { rawTx :: ByteString
          }
    deriving (Show, Ord, Eq, Read, Generic)

instance ToJSON NexaRequest where
    toJSON (AuthenticateRequest u p) = object ["username" .= u, "password" .= p]
    toJSON (NameOutpointRequest n i) = object ["name" .= n, "isProducer" .= i]
    toJSON (GetUTXOsByAddressRequest a p) = object ["address" .= a, "pageSize" .= p]
    toJSON (RelayTxRequest r) = object ["rawTx" .= (T.decodeUtf8 $ B64.encode r)]

data AuthenticateResponse =
    AuthenticateResponse
        { auth :: AuthenticateResponse'
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON AuthenticateResponse

data AuthenticateResponse' =
    AuthenticateResponse'
        { sessionKey :: Maybe String
        , callsUsed :: Int
        , callsRemaining :: Int
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON AuthenticateResponse'

data NameOutpointResponse =
    NameOutpointResponse
        { forName :: [Int]
        , script :: String
        , outPoint :: OutPoint'
        , isProducer :: Bool
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON NameOutpointResponse

data GetUtxosByAddressResponse =
    GetUtxosByAddressResponse
        { nextCursor :: Maybe String
        , utxos :: [AddressOutputs]
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON GetUtxosByAddressResponse

data RelayTxResponse =
    RelayTxResponse
        { txBroadcast :: Bool
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON RelayTxResponse

data OutPoint' =
    OutPoint'
        { opTxHash :: String
        , opIndex :: Int
        }
    deriving (Show, Ord, Eq, Read, Generic, Serialise)

instance FromJSON OutPoint'

instance ToJSON OutPoint'

data AddressOutputs =
    AddressOutputs
        { address :: String
        , outputTxHash :: String
        , outputIndex :: Int
        , txIndex :: Int
        , blockHash :: String
        , blockHeight :: Int
        , spendInfo :: Maybe SpendInfo
        , prevOutpoint :: [(OutPoint', Int32, Int64)]
        , value :: Int64
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON AddressOutputs

data SpendInfo =
    SpendInfo
        { spendingTxId :: String
        , spendingTxIndex :: Int32
        , spendingBlockHash :: String
        , spendingBlockHeight :: Int32
        , spendData :: [SpendInfo']
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON SpendInfo

data SpendInfo' =
    SpendInfo'
        { spendingOutputIndex :: Int32
        , outputAddress :: T.Text
        , value' :: Int64
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON SpendInfo'
