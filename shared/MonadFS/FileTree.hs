{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MonadFS.FileTree where

import           Data.Aeson
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           GHC.Generics
import           MonadFS.API.Types
import           System.Directory.Tree (DirTree (..))

data FileTree a = FileTree {
    directories :: Map DirName (FileTree a)
  , files       :: Map FileName a
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


instance Functor FileTree where
  fmap f (FileTree ds fs) = FileTree (fmap f <$> ds) (f <$> fs)


data FileNode = FileNode {
    fileName :: FileName
  , fileInfo :: FileInfo
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


newtype DirName = DirName String
  deriving (Eq, Ord, Show, Generic, FromJSON,
            ToJSON, FromJSONKey, ToJSONKey)


type VFS = FileTree FileInfo

type StorageTree = FileTree ()




dirTreeToFileTree :: DirTree a -> FileTree a
dirTreeToFileTree (Failed _ _) = FileTree Map.empty Map.empty
dirTreeToFileTree (File name' p) = FileTree Map.empty (Map.singleton (FileName name') p)
dirTreeToFileTree (Dir _ contents') = FileTree (Map.fromList dirs') (Map.fromList files')
  where
    files' = (\(File name' p) -> (FileName name', p))
      <$> filter isFile contents'

    dirs' = (\d@(Dir name' _) -> (DirName name', dirTreeToFileTree d))
      <$> filter isDir contents'

    isFile (File _ _)   = True
    isFile (Dir _ _)    = False
    isFile (Failed _ _) = False

    isDir (File _ _)   = False
    isDir (Dir _ _)    = True
    isDir (Failed _ _) = False
