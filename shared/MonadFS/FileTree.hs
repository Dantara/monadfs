{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MonadFS.FileTree where

import           Data.Aeson
import           Data.ByteString
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map
import           GHC.Generics
import           MonadFS.API.Types

data FileTree a = FileTree {
    directories :: Map DirName (FileTree a)
  , files       :: Map FileName a
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


data FileNode = FileNode {
    fileName :: FileName
  , fileInfo :: FileInfo
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


newtype DirName = DirName String
  deriving (Eq, Ord, Show, Generic, FromJSON,
            ToJSON, FromJSONKey, ToJSONKey)


type VFS = FileTree FileNode

type StorageTree = FileTree FileName
