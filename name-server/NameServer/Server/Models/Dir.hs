{-# LANGUAGE OverloadedStrings #-}

module NameServer.Server.Models.Dir where

import qualified Data.Map.Strict               as Map
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           MonadFS.API.Types
import           MonadFS.FileTree
import           NameServer.Server.Models.Root


createDir :: DirPath -> VFS -> Either DirError VFS
createDir (DirPath ('/':xs)) tree = createDir (DirPath xs) tree
createDir (DirPath path) tree
  | null path = Left IncorrectDirPath
  | null tail' || tail' == "/" =
    case (Map.lookup dirName' dirs, Map.lookup fileName' fs) of
      (Nothing, Nothing) ->
        Right $ tree { directories = Map.insert dirName' initVFS dirs }
      (Just _, _) ->
        Left DirExists
      (Nothing, Just _) ->
        Left $ CustomDirError "File with the same name already exists"
  | otherwise = case Map.lookup dirName' dirs of
      Just subTree ->
          either
            Left
            (\t -> Right $ tree { directories = Map.update (\_ -> Just t) dirName' dirs })
            (createDir (DirPath tail') subTree)
      Nothing ->
        Left IncorrectDirPath
    where
      name = takeWhile (/= '/') path
      tail' = dropWhile (/= '/') path
      fileName' = FileName name
      dirName' = DirName name
      dirs = directories tree
      fs = files tree


deleteDir :: DirPath -> VFS -> Either DirError ([ServerAddr], VFS)
deleteDir (DirPath ('/':xs)) tree = deleteDir (DirPath xs) tree
deleteDir (DirPath path) tree
  | null path = Left IncorrectDirPath
  | (null tail' || tail' == "/") && Map.member dirName' dirs =
        Right (addrs tree, tree { directories = Map.delete dirName' dirs })
  | otherwise = case Map.lookup dirName' dirs of
      Just subTree ->
          either
            Left
            (\(list, t) -> Right (list, tree {
                             directories = Map.update (\_ -> Just t) dirName' dirs
                                             }))
            (deleteDir (DirPath tail') subTree)
      Nothing ->
        Left DirDoesNotExist
    where
      name = takeWhile (/= '/') path
      tail' = dropWhile (/= '/') path
      dirName' = DirName name
      dirs = directories tree
      addrs t = extractAddrsFromVFS t


extractAddrsFromVFS :: VFS -> [ServerAddr]
extractAddrsFromVFS tree = Set.toList $ go tree Set.empty
  where
    go :: VFS -> Set ServerAddr -> Set ServerAddr
    go t s
      | Map.null (directories t) =
          s <> fromCurrentDir t
      | otherwise =
        s <> (mconcat $ Map.elems
                      $ (`go` s) <$> directories t)
          <> fromCurrentDir t
    fromCurrentDir t =
      Set.fromList
      $ concat
      $ (\(FileInfo _ xs) -> xs)
      <$> Map.elems (files t)


getDirInfo :: DirPath -> VFS -> Either DirError DirInfo
getDirInfo path tree = DirInfo . Map.keys . files
  <$> findDir path tree


findDir :: DirPath -> VFS -> Either DirError VFS
findDir (DirPath "/") tree = Right tree
findDir (DirPath ('/':xs)) tree = findDir (DirPath xs) tree
findDir (DirPath path) tree
  | null path = Left IncorrectDirPath
  | null tail' || tail' == "/" = maybe
    (Left DirDoesNotExist)
    Right
    (Map.lookup dirName' dirs)
  | otherwise = maybe
    (Left IncorrectDirPath)
    (findDir (DirPath tail'))
    (Map.lookup dirName' dirs)
    where
      name = takeWhile (/= '/') path
      tail' = dropWhile (/= '/') path
      dirName' = DirName name
      dirs = directories tree
