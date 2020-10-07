{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MonadFS.API.Types where

import           Data.Aeson
import           Data.ByteString
import           Data.Text
import           GHC.Generics

data FileStatus
  = FileSuccess
  | FileError FileError
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


data FileError
  = FileExists
  | FileDoesNotExist
  | IncorrectFilePath
  | CustomFileError Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


data DirStatus
  = DirSuccess
  | DirError DirError
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


data DirError
  = DirExists
  | DirDoesNotExist
  | IncorrectDirPath
  | CustomDirError Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data StorageServerStatus
  = StorageServerOk Size
  | StorageServerError Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data SystemStatus
  = SystemOk Size
  | SystemError SystemError
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype Size = Size Integer
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


data SystemError
  = NoStorageServersAvaliable
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ServerAddr = ServerAddr String Int
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


data FileInfo = FileInfo Size [ServerAddr]
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


newtype DirPath = DirPath String
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


newtype FileName = FileName String
  deriving (Eq, Ord, Show, Generic, FromJSON,
            ToJSON, FromJSONKey, ToJSONKey)


newtype DirInfo = DirInfo [FileName]
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
