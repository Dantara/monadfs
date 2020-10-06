{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module MonadFS.API.NameServer where

import           Data.Aeson
import           Data.ByteString
import           MonadFS.API.Types
import           Servant.API

type NameServerAPI = "init" :> Get '[JSON] SystemStatus
                :<|> "file" :> FileAPI
                :<|> "dir"  :> DirAPI

type FileAPI = "create" :> ReqBody '[JSON] FilePath   :> Post '[JSON] FileStatus
          :<|> "read"   :> ReqBody '[JSON] FilePath   :> Post '[JSON] IP
          :<|> "write"  :> ReqBody '[JSON] FilePath   :> Post '[JSON] IPList
          :<|> "delete" :> ReqBody '[JSON] FilePath   :> Post '[JSON] FileStatus
          :<|> "info"   :> QueryParam "file" FilePath :> Get '[JSON] FileInfo
          :<|> "copy"   :> ReqBody '[JSON] FilePath   :> Post '[JSON] FileStatus
          :<|> "move"   :> ReqBody '[JSON] FilePath   :> Post '[JSON] FileStatus

type DirAPI = "create" :> ReqBody '[JSON] DirPath  :> Post '[JSON] DirStatus
         :<|> "delete" :> ReqBody '[JSON] DirPath  :> Post '[JSON] DirStatus
         :<|> "info"   :> QueryParam "dir" DirPath :> Get '[JSON] DirInfo
         :<|> "exists" :> QueryParam "dir" DirPath :> Get '[JSON] DirStatus

