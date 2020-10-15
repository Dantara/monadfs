{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NameServer.Types where

import           Control.Concurrent.STM.TVar
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           GHC.Generics
import           MonadFS.API.Types
import           MonadFS.FileTree
import           Network.HTTP.Client         (Manager)
import           Servant


newtype AppM a = AppM { runAppM :: ReaderT AppState Handler a }
  deriving (Functor)
  deriving newtype (
             Applicative
           , Monad
           , MonadReader AppState
           , MonadIO )


data AppState = AppState {
    fileTree         :: TVar VFS
  , avaliableSSs     :: TVar [StorageServer]
  , ssAddrs          :: [ServerAddr]
  , globalManager    :: Manager
  , amountOfReplicas :: Int
  }


data StorageServer = StorageServer {
    ssAddr :: ServerAddr
  , ssSize :: Size
  } deriving (Eq, Ord)


newtype RequestM a = RequestM { runRequestM :: ReaderT Manager IO a }
  deriving (Functor)
  deriving newtype (
             Applicative
           , Monad
           , MonadReader Manager
           , MonadIO )

proceedRequestM :: Manager -> RequestM a -> IO a
proceedRequestM mng req = runReaderT (runRequestM req) mng


data Config = Config {
    port                      :: Int
  , storageServersAddrs       :: [ServerAddr]
  , replicasAmount            :: Int
  , fileTreeCachePeriod       :: Int
  , fileTreeCacheFile         :: FilePath
  , avaliablityCheckingPeriod :: Int
} deriving (Show, Generic, FromJSON)


data FixCommand
  = CreateDir DirPath
  | RemoveDir DirPath
  | RemoveFile FilePath
  | LoadMissingFile LoadFile
  deriving (Eq, Show)
