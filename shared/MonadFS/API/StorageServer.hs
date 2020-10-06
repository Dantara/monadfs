{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module MonadFS.API.StorageServer where

import           MonadFS.API.Types
import           MonadFS.FileTree
import           Servant.API

type StorageServerAPI =
  "init" :> Get '[JSON] SystemStatus
    :<|> "tree" :> Get '[JSON] StorageTree
    :<|> "status" :> Get '[JSON] SystemStatus
    :<|> "file" :> FileAPI
    :<|> "dir" :> DirAPI

type FileAPI =
  "create" :> ReqBody '[JSON] FilePath :> Post '[JSON] FileStatus
    :<|> "read" :> ReqBody '[JSON] FilePath :> Post '[JSON] Raw
    :<|> "write" :> ReqBody '[JSON] FilePath :> Post '[JSON] Raw
    :<|> "delete" :> ReqBody '[JSON] FilePath :> Post '[JSON] FileStatus
    :<|> "copy" :> ReqBody '[JSON] FilePath :> Post '[JSON] FileStatus
    :<|> "move" :> ReqBody '[JSON] FilePath :> Post '[JSON] FileStatus

type DirAPI =
  "create" :> ReqBody '[JSON] DirPath :> Post '[JSON] DirStatus
    :<|> "delete" :> ReqBody '[JSON] DirPath :> Post '[JSON] DirStatus
