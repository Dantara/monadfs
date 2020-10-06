module System.FileTree where

import           API.Internals
import           Data.ByteString
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data FileTree a
  = Dir (Map ByteString (FileTree a))
  | File a

data FileNode = FileNode ByteString [IP]

type VFS = FileTree FileNode
