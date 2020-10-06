module MonadFS.FileTree where

import           Data.ByteString
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map
import           MonadFS.API.Types

data FileTree a
  = Dir (Map String (FileTree a))
  | Files (Map String a)

data FileNode = FileNode String [IP]

type VFS = FileTree FileNode

type StorageTree = FileTree FileName
