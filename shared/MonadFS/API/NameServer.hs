{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module MonadFS.API.NameServer where

import           MonadFS.API.Types
import           Servant.API

type NameServerAPI = "init" :> Get '[JSON] SystemStatus
                :<|> "file" :> FileAPI
                :<|> "dir"  :> DirAPI

type FileAPI = "create" :> ReqBody '[JSON] FilePath   :> Post '[JSON] (FileStatus ())
          :<|> "read"   :> ReqBody '[JSON] FilePath   :> Post '[JSON] (FileStatus ServerAddr)
          :<|> "write"  :> ReqBody '[JSON] NewFile    :> Post '[JSON] (FileStatus [ServerAddr])
          :<|> "delete" :> ReqBody '[JSON] FilePath   :> Post '[JSON] (FileStatus ())
          :<|> "info"   :> QueryParam' '[Required] "file" FilePath
               :> Get '[JSON] (FileStatus FileInfo)
          :<|> "copy"   :> ReqBody '[JSON] FilePath   :> Post '[JSON] (FileStatus ())
          :<|> "move"   :> ReqBody '[JSON] FilePath   :> Post '[JSON] (FileStatus ())

type DirAPI = "create" :> ReqBody '[JSON] DirPath  :> Post '[JSON] (DirStatus ())
         :<|> "delete" :> ReqBody '[JSON] DirPath  :> Post '[JSON] (DirStatus ())
         :<|> "info"   :> QueryParam' '[Required] "dir" DirPath :> Get '[JSON] (DirStatus DirInfo)
         :<|> "exists" :> QueryParam' '[Required] "dir" DirPath :> Get '[JSON] (DirStatus ())
