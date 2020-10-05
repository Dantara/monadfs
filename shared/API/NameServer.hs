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

type FileAPI = "create" :> ReqBody '[JSON] ByteString :> Post '[JSON] FileStatus

type DirAPI = "create" :> ReqBody '[JSON] ByteString :> Post '[JSON] DirStatus

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

data SystemStatus = SystemStatus SystemSize

newtype SystemSize = SystemSize Integer

