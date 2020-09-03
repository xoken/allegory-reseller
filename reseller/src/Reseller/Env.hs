{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}

module Reseller.Env where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.Secp256k1
import qualified Data.HashTable.IO as H
import Data.Hashable
import Data.Int
import qualified Data.Map.Strict as M
import Data.Text
import Data.Time.Clock
import Data.Word
import Database.LevelDB
import GHC.Generics
import NodeConfig
import Prelude
import System.Logger

type HashTable k v = H.BasicHashTable k v

type HasResellerEnv env m
     = ( HasBitcoinP2P m
       , HasDatabaseHandles m
       , HasLogger m
       , HasAllegoryEnv m
       , MonadReader env m
       , MonadBaseControl IO m
       , MonadThrow m)

data ResellerEnv =
    ResellerEnv
        { loggerEnv :: !Logger
        , bitcoinP2PEnv :: !BitcoinP2P
        , allegoryEnv :: !AllegoryEnv
        , dbHandles :: !DatabaseHandles
        }

data BitcoinP2P =
    BitcoinP2P
        { nodeConfig :: NodeConfig
        , userDataCache :: !(HashTable Text (Text, Int32, Int32, UTCTime, [Text])) -- (name, quota, used, expiry time, roles)
        }

data AllegoryEnv =
    AllegoryEnv
        { allegorySecretKey :: !SecKey
        }

data DatabaseHandles =
    DatabaseHandles
        { leveldb :: !ServerState
        }

data ServerState =
    ServerState
        { writeOptions :: !WriteOptions
        , readOptions :: !ReadOptions
        , db :: !DB
        }

class HasBitcoinP2P m where
    getBitcoinP2P :: m (BitcoinP2P)

class HasLogger m where
    getLogger :: m (Logger)

class HasAllegoryEnv m where
    getAllegory :: m (AllegoryEnv)

class HasDatabaseHandles m where
    getDB :: m (DatabaseHandles)
