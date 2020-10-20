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
  "create" :> ReqBody '[JSON] AbsFilePath :> Post '[JSON] (FileStatus ())
    :<|> "read" :> QueryParam' '[Required] "file" AbsFilePath
                      :> Get '[OctetStream] FileContent
    :<|> "write" :> ReqBody '[OctetStream] FileWrite :> Post '[JSON] (FileStatus ())
    :<|> "delete" :> ReqBody '[JSON] AbsFilePath :> Post '[JSON] (FileStatus ())
    :<|> "copy" :> ReqBody '[JSON] SourceDest :> Post '[JSON] (FileStatus ())
    :<|> "move" :> ReqBody '[JSON] SourceDest :> Post '[JSON] (FileStatus ())
    :<|> "load" :> ReqBody '[JSON] LoadFile :> Post '[JSON] (FileStatus ())

type DirAPI =
  "create" :> ReqBody '[JSON] AbsDirPath :> Post '[JSON] (DirStatus ())
    :<|> "delete" :> ReqBody '[JSON] AbsDirPath :> Post '[JSON] (DirStatus ())
