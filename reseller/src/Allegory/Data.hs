{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Allegory.Data where

import Codec.Serialise
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Yaml ()
import GHC.Generics

data Allegory =
    Allegory
        { version :: !Int
        , name :: ![Int]
        , action :: !Action
        }
    deriving (Show, Generic, Eq, Serialise)

data Action
    = ProducerAction
          { producerInput :: !Index
          , producerOutput :: !ProducerOutput
          , pOwnerOutput :: !(Maybe OwnerOutput)
          , extensions :: ![Extension]
          }
    | OwnerAction
          { ownerInput :: !Index
          , ownerOutput :: !OwnerOutput
          , oProxyProviders :: ![ProxyProvider]
          }
    deriving (Show, Generic, Eq, Serialise)

data ProducerOutput =
    ProducerOutput
        { producer :: !Index
        , pVendorEndpoint :: !(Maybe Endpoint)
        }
    deriving (Show, Generic, Eq, Serialise)

data OwnerOutput =
    OwnerOutput
        { owner :: !Index
        , oVendorEndpoint :: !(Maybe Endpoint)
        }
    deriving (Show, Generic, Eq, Serialise)

data Index =
    Index
        { index :: !Int
        }
    deriving (Show, Generic, Eq, Serialise)

data Extension
    = OwnerExtension
          { ownerOutputEx :: !OwnerOutput
          , codePoint :: !Int
          }
    | ProducerExtension
          { producerOutputEx :: !ProducerOutput
          , codePoint :: !Int
          }
    deriving (Show, Generic, Eq, Serialise)

data ProxyProvider =
    ProxyProvider
        { service :: !String
        , mode :: !String
        , endpoint :: !Endpoint
        , registration :: !Registration
        }
    deriving (Show, Generic, Eq, Serialise)

instance ToJSON ProxyProvider

data Endpoint =
    Endpoint
        { protocol :: !String
        , uri :: !String
        }
    deriving (Show, Generic, Eq, Serialise)

instance ToJSON Endpoint

data Registration =
    Registration
        { addressCommitment :: !String
        , utxoCommitment :: !String
        , publicKeyAuthEncrypt :: !String
        , expiry :: !Int
        }
    deriving (Show, Generic, Eq, Serialise)

instance ToJSON Registration

frameOpReturn :: C.ByteString -> C.ByteString
frameOpReturn opReturn = do
    let prefix = (fst . B16.decode) "006a0f416c6c65676f72792f416c6c506179"
    let len = B.length opReturn
    let xx =
            if (len <= 0x4b)
                then word8 $ fromIntegral len
                else if (len <= 0xff)
                         then mappend (word8 0x4c) (word8 $ fromIntegral len)
                         else if (len <= 0xffff)
                                  then mappend (word8 0x4d) (word16LE $ fromIntegral len)
                                  else if (len <= 0x7fffffff)
                                           then mappend (word8 0x4e) (word32LE $ fromIntegral len)
                                           else word8 0x99 -- error scenario!!
    let bs = LC.toStrict $ toLazyByteString xx
    C.append (C.append prefix bs) opReturn
