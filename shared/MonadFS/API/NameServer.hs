{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module MonadFS.API.NameServer where

import           MonadFS.API.Types
import           Servant.API

type NameServerAPI = "init" :> Get '[JSON] SystemStatus
                :<|> "file" :> FileAPI
                :<|> "dir"  :> DirAPI

type FileAPI = "create" :> ReqBody '[JSON] AbsFilePath   :> Post '[JSON] (FileStatus ())
          :<|> "read"   :> ReqBody '[JSON] AbsFilePath   :> Post '[JSON] (FileStatus ServerAddr)
          :<|> "write"  :> ReqBody '[JSON] NewFile    :> Post '[JSON] (FileStatus [ServerAddr])
          :<|> "delete" :> ReqBody '[JSON] AbsFilePath   :> Post '[JSON] (FileStatus ())
          :<|> "info"   :> QueryParam' '[Required] "file" AbsFilePath
               :> Get '[JSON] (FileStatus FileInfo)
          :<|> "copy"   :> ReqBody '[JSON] SourceDest :> Post '[JSON] (FileStatus ())
          :<|> "move"   :> ReqBody '[JSON] SourceDest :> Post '[JSON] (FileStatus ())

type DirAPI = "create" :> ReqBody '[JSON] AbsDirPath  :> Post '[JSON] (DirStatus ())
         :<|> "delete" :> ReqBody '[JSON] AbsDirPath  :> Post '[JSON] (DirStatus ())
         :<|> "info"   :> QueryParam' '[Required] "dir" AbsDirPath :> Get '[JSON] (DirStatus DirInfo)
         :<|> "exists" :> QueryParam' '[Required] "dir" AbsDirPath :> Get '[JSON] (DirStatus ())
