{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Nexa.Types where

-- import qualified Codec.Serialise as CBOR
import Codec.Serialise
import Control.Exception
import Data.Aeson
import Data.ByteString as BS
import Data.ByteString.Base64 as B64
import Data.ByteString.Char8 as B8 (pack)
import Data.Hashable
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import GHC.Generics
import Network.Xoken.Transaction

data NexaException
    = AuthException
    | ResponseParseException
    | ServerException
    | EmptyResponseException
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
        , isConfirmed :: Bool
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
        , txIndex :: Maybe Int
        , blockHash :: Maybe String
        , blockHeight :: Maybe Int
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

data Tx' =
    Tx'
        { txVersion :: !Word32
        , txIn :: ![TxIn']
        , txOut :: ![TxOut]
        , txLockTime :: !Word32
        }
    deriving (Show, Read, Eq, Ord, Generic, Hashable, Serialise)

data TxIn' =
    TxIn'
        { prevOutput :: !OutPoint
        , scriptInput :: !ByteString
        , txInSequence :: !Word32
        , value :: !Word64
        }
    deriving (Eq, Show, Read, Ord, Generic, Hashable, Serialise)

instance ToJSON Tx' where
    toJSON (Tx' v i o l) = object ["version" .= v, "ins" .= i, "outs" .= o, "locktime" .= l]

instance ToJSON TxIn' where
    toJSON (TxIn' op scr seq val) = object ["outpoint" .= op, "script" .= scr, "sequence" .= seq, "value" .= val]
