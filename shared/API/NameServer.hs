{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module API.NameServer where

import           Data.Aeson
import           Data.ByteString
import           Servant.API

-- Here we will define api for Name Server

type NameServerAPI = "init" :> Get '[JSON] SystemStatus
                :<|> "file" :> FileAPI
                :<|> "dir" :> DirAPI

type FileAPI = "create" :> ReqBody '[JSON] FilePath :> Post '[JSON] FileStatus
          :<|> "read" :> ReqBody '[JSON] FilePath :> Post '[JSON] IP
          :<|> "write" :> ReqBody '[JSON] FilePath :> Post '[JSON] IPList
          :<|> "delete" :> ReqBody '[JSON] FilePath :> Post '[JSON] FileStatus
          :<|> "info" :> QueryParam "file" FilePath :> Get '[JSON] FileInfo
          :<|> "copy" :> ReqBody '[JSON] FilePath :> Post '[JSON] FileStatus
          :<|> "move" :> ReqBody '[JSON] FilePath :> Post '[JSON] FileStatus

type DirAPI = "create" :> ReqBody '[JSON] DirPath :> Post '[JSON] DirStatus
         :<|> "delete" :> ReqBody '[JSON] DirPath :> Post '[JSON] DirStatus
         :<|> "info" :> QueryParam "dir" DirPath :> Get '[JSON] DirInfo
         :<|> "exists" :> QueryParam "dir" DirPath :> Get '[JSON] DirStatus

data FileStatus = FileSuccess | FileError FileError

data FileError
  = FileExists
  | FileDoesNotExist
  | IncorrectFilePath
  | CustomFileError ByteString

data DirStatus = DirSuccess | DirError DirError

data DirError
  = DirExists
  | DirDoesNotExist
  | IncorrectDirPath
  | CustomDirError ByteString

newtype SystemStatus = SystemStatus Size

newtype Size = Size Integer

newtype IP = IP ByteString
newtype IPList = IPList [IP]

newtype ServerName = ServerName ByteString

data FileInfo = FileInfo Size [ServerName]

newtype DirPath = DirPath String
newtype FileName = FileName ByteString

newtype DirInfo = DirInfo [FileName]
