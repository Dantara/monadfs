module MonadFS.API.Types where

import           Data.ByteString

data FileStatus
  = FileSuccess
  | FileError FileError

data FileError
  = FileExists
  | FileDoesNotExist
  | IncorrectFilePath
  | CustomFileError ByteString

data DirStatus
  = DirSuccess
  | DirError DirError

data DirError
  = DirExists
  | DirDoesNotExist
  | IncorrectDirPath
  | CustomDirError ByteString

newtype SystemStatus = SystemStatus Size

newtype Size = Size Integer

newtype IP = IP ByteString
newtype IPList = IPList [IP]

data FileInfo = FileInfo Size [IP]

newtype DirPath = DirPath String
newtype FileName = FileName ByteString

newtype DirInfo = DirInfo [FileName]

newtype StorageState = StorageState [String]
