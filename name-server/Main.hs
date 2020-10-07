{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Concurrent.STM.TVar
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           MonadFS.API.NameServer
import           MonadFS.API.StorageServer
import           MonadFS.API.Types
import           MonadFS.FileTree
import           Network.HTTP.Client         (Manager)
import           Servant
import           Servant.Client

nameServerPort :: Int
nameServerPort = 4000

newtype AppM a = AppM { runAppM :: ReaderT AppState Handler a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader AppState
           , MonadIO
           )

data AppState = AppState {
    fileTree     :: TVar VFS
  , avaliableSSs :: TVar (Set StorageServer)
  , ssAddrs      :: [ServerAddr]
  } deriving (Eq)

data StorageServer = StorageServer {
    ssAddr :: ServerAddr
  , ssSize :: Size
  }

newtype RequestM a = RequestM { runRequestM :: ReaderT Manager IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Manager
           , MonadIO
           )

main :: IO ()
main = do
  putStrLn $ "[+] Starting Name Server on " <> show nameServerPort <> " port."

nameServer :: AppM ()
nameServer = undefined

initVFS :: VFS
initVFS = FileTree Map.empty Map.empty

nameServerApi :: Proxy NameServerAPI
nameServerApi = Proxy

storageServerApi :: Proxy StorageServerAPI
storageServerApi = Proxy

server :: ServerT NameServerAPI AppM
server = initController
  :<|> (fileCreateController :<|> fileReadController
       :<|> fileWriteController :<|> fileDeleteController
       :<|> fileInfoController :<|> fileCopyController
       :<|> fileMoveController)
  :<|> (dirCreateController :<|> dirDeleteController
       :<|> dirInfoController :<|> dirExistsController)

-- | Controllers

initController :: AppM SystemStatus
initController = undefined


fileCreateController :: FilePath -> AppM FileStatus
fileCreateController = undefined

fileReadController :: FilePath -> AppM ServerAddr
fileReadController = undefined

fileWriteController :: FilePath -> AppM [ServerAddr]
fileWriteController = undefined

fileDeleteController :: FilePath -> AppM FileStatus
fileDeleteController = undefined

fileInfoController :: FilePath -> AppM FileInfo
fileInfoController = undefined

fileCopyController :: FilePath -> AppM FileStatus
fileCopyController = undefined

fileMoveController :: FilePath -> AppM FileStatus
fileMoveController = undefined


dirCreateController :: DirPath -> AppM DirStatus
dirCreateController = undefined

dirDeleteController :: DirPath -> AppM DirStatus
dirDeleteController = undefined

dirInfoController :: DirPath -> AppM DirInfo
dirInfoController = undefined

dirExistsController :: DirPath -> AppM DirStatus
dirExistsController = undefined

-- | Clients

initClient :<|> treeClient :<|> statusClient
  :<|> (fileCreateClient :<|> fileReadClient :<|> fileWriteFile
       :<|> fileDeleteClient :<|> fileCopyClient :<|> fileMoveClient)
  :<|> (dirCreateClient :<|> dirDeleteClient) = client storageServerApi

-- | Helpers

checkSS :: ServerAddr -> RequestM (Maybe StorageServer)
checkSS addr@(ServerAddr url port) = do
  mng <- ask
  res <- liftIO $ runClientM statusClient $ mkClientEnv mng bUrl
  case res of
    Right (StorageServerOk s) ->
      pure $ Just $ StorageServer addr s
    _ ->
      pure Nothing
    where
      bUrl = BaseUrl Http url port ""


lookupSSs :: [ServerAddr] -> RequestM (Set StorageServer)
lookupSSs addrs = mapM checkSS addrs
  >>= (pure . Set.fromList . (map (\(Just x) -> x)) . (filter mbToBool))
  where
    mbToBool (Just _) = True
    mbToBool Nothing  = False
