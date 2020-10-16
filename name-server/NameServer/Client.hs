module NameServer.Client where

import           MonadFS.API.StorageServer
import           MonadFS.API.Types
import           MonadFS.FileTree
import           Servant
import           Servant.Client


initClient :: ClientM StorageServerStatus
treeClient :: ClientM StorageTree
statusClient :: ClientM StorageServerStatus
fileCreateClient :: FilePath -> ClientM (FileStatus ())
fileDeleteClient :: FilePath -> ClientM (FileStatus ())
fileCopyClient :: SourceDest -> ClientM (FileStatus ())
fileMoveClient :: SourceDest -> ClientM (FileStatus ())
fileLoadClient :: LoadFile -> ClientM (FileStatus ())
dirCreateClient :: DirPath -> ClientM (DirStatus ())
dirDeleteClient :: DirPath -> ClientM (DirStatus ())


storageServerApi :: Proxy StorageServerAPI
storageServerApi = Proxy


initClient :<|> treeClient :<|> statusClient
  :<|> (fileCreateClient :<|> _ :<|> _
       :<|> fileDeleteClient :<|> fileCopyClient
       :<|> fileMoveClient :<|> fileLoadClient)
  :<|> (dirCreateClient :<|> dirDeleteClient) = client storageServerApi
