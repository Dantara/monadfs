{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MonadFS.FileTree where

import           Data.Aeson
import           Data.ByteString
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map
import           GHC.Generics
import           MonadFS.API.Types

data FileTree a
  = Dir (Map String (FileTree a))
  | Files (Map String a)
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data FileNode = FileNode String [IP]
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

type VFS = FileTree FileNode

type StorageTree = FileTree FileName
