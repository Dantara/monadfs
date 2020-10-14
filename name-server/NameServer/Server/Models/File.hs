{-# LANGUAGE OverloadedStrings #-}

module NameServer.Server.Models.File where

import qualified Data.Map.Strict   as Map
import           MonadFS.API.Types
import           MonadFS.FileTree


addFileToTree :: NewFile -> [ServerAddr] -> VFS -> Either FileError VFS
addFileToTree (NewFile ('/':xs) s) addrs tree = addFileToTree (NewFile xs s) addrs tree
addFileToTree (NewFile path size) addrs tree
  | null path = Left IncorrectFilePath
  | null name = Left $ CustomFileError "Empty file name"
  | Map.member fileName' $ files tree = Left FileExists
  | null tail' = Right
    $ tree { files = Map.insert fileName' fileInfo' (files tree) }
  | otherwise = case Map.lookup dirName' dirs of
      Just subTree ->
          either
            Left
            (\t -> Right $ tree { directories = Map.update (\_ -> Just t) dirName' dirs })
            (addFileToTree (NewFile tail' size) addrs subTree)
      Nothing ->
        Left IncorrectFilePath
    where
      name = takeWhile (/= '/') path
      tail' = dropWhile (/= '/') path
      fileName' = FileName name
      dirName' = DirName name
      fileInfo' = FileInfo size addrs
      dirs = directories tree


findFileNode :: FilePath -> VFS -> Either FileError FileNode
findFileNode ('/':xs) tree = findFileNode xs tree
findFileNode path tree
  | null path = Left IncorrectFilePath
  | null name = Left $ CustomFileError "Empty file name"
  | null tail' = maybe
    (Left FileDoesNotExist)
    (\i -> Right $ FileNode fileName' i)
    (Map.lookup fileName' (files tree))
  | null dirs = Left FileDoesNotExist
  | otherwise = maybe
    (Left IncorrectFilePath)
    (findFileNode tail')
    (Map.lookup dirName' dirs)
    where
      name = takeWhile (/= '/') path
      tail' = dropWhile (/= '/') path
      fileName' = FileName name
      dirName' = DirName name
      dirs = directories tree


deleteFileNode :: FilePath -> VFS -> Either FileError (FileNode, VFS)
deleteFileNode ('/':xs) tree = deleteFileNode xs tree
deleteFileNode path tree
  | null path = Left IncorrectFilePath
  | Map.member fileName' $ files tree = Left FileExists
  | null tail' = maybe
    (Left FileDoesNotExist)
    (\x -> Right (FileNode fileName' x, tree { files = Map.delete fileName' fs }))
    (Map.lookup fileName' fs)
  | otherwise = case Map.lookup dirName' dirs of
      Just subTree ->
          either
            Left
            (\(n, t) -> Right (n, tree { directories = Map.update (\_ -> Just t) dirName' dirs }))
            (deleteFileNode path subTree)
      Nothing ->
        Left IncorrectFilePath
    where
      name = takeWhile (/= '/') path
      tail' = dropWhile (/= '/') path
      fileName' = FileName name
      dirName' = DirName name
      dirs = directories tree
      fs = files tree
