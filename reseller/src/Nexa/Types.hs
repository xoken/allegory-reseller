{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Nexa.Types where

import Codec.Serialise
import Control.Exception
import Data.Aeson
import Data.Int
import qualified Data.Text as T
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
    | GetProducerRequest
          { name :: [Int]
          }
    | GetUTXOsByAddressRequest
          { address :: String
          , pageSize :: Int
          }
    deriving (Show, Ord, Eq, Read, Generic)

instance ToJSON NexaRequest

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

data GetProducerResponse =
    GetProducerResponse
        { name :: [Int]
        , script :: String
        , outpoint :: OutPoint'
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON GetProducerResponse

data GetUtxosByAddressResponse =
    GetUtxosByAddressResponse
        { nextCursor :: Maybe String
        , utxos :: [AddressOutputs]
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON GetUtxosByAddressResponse

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
