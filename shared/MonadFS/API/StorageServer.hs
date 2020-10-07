{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module MonadFS.API.StorageServer where

import           MonadFS.API.Types
import           MonadFS.FileTree
import           Servant.API
import           Servant.Multipart

type StorageServerAPI =
  "init" :> Get '[JSON] StorageServerStatus
    :<|> "tree" :> Get '[JSON] StorageTree
    :<|> "status" :> Get '[JSON] StorageServerStatus
    :<|> "file" :> FileAPI
    :<|> "dir" :> DirAPI

type FileAPI =
  "create" :> ReqBody '[JSON] FilePath :> Post '[JSON] FileStatus
    :<|> "read" :> Raw
    :<|> "write" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] FileStatus
    :<|> "delete" :> ReqBody '[JSON] FilePath :> Post '[JSON] FileStatus
    :<|> "copy" :> ReqBody '[JSON] FilePath :> Post '[JSON] FileStatus
    :<|> "move" :> ReqBody '[JSON] FilePath :> Post '[JSON] FileStatus

type DirAPI =
  "create" :> ReqBody '[JSON] DirPath :> Post '[JSON] DirStatus
    :<|> "delete" :> ReqBody '[JSON] DirPath :> Post '[JSON] DirStatus

