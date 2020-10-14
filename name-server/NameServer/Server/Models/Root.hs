module NameServer.Server.Models.Root where

import           Data.Map.Strict  as Map
import           MonadFS.FileTree

initVFS :: VFS
initVFS = FileTree Map.empty Map.empty
