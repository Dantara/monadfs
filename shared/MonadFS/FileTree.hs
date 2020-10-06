module MonadFS.FileTree where

import           Data.ByteString
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map
import           MonadFS.API.Types

data FileTree a
  = Dir (Map ByteString (FileTree a))
  | File a

data FileNode = FileNode ByteString [IP]

type VFS = FileTree FileNode
