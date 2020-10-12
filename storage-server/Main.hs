{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (catch)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List (stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybe)
import qualified Data.Text as T
import MonadFS.API.StorageServer (StorageServerAPI)
import MonadFS.API.Types
import MonadFS.FileTree (DirName (DirName), FileTree (FileTree))
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Multipart
import Servant.Multipart (MultipartData, Tmp)
import System.Directory
import System.Directory.Tree (AnchoredDirTree ((:/)), DirTree (Dir, Failed, File), build)
import System.DiskSpace (getAvailSpace)

baseLocalDir :: FilePath
baseLocalDir = "/monadfs/storage"

storageServerPort :: Int
storageServerPort = 4000

storageServerApi :: Proxy StorageServerAPI
storageServerApi = Proxy

storageServer :: Server StorageServerAPI
storageServer =
  initController
    :<|> treeController
    :<|> statusController
    :<|> ( fileCreateController
             :<|> fileReadController
             :<|> fileWriteController
             :<|> fileDeleteController
             :<|> fileCopyController
             :<|> fileMoveController
             :<|> fileLoadController
         )
    :<|> ( dirCreateController :<|> dirDeleteController
         )

main :: IO ()
main = do
  putStrLn $ "[+] Starting Storage Server on " <> show storageServerPort <> " port."
  run storageServerPort (serve storageServerApi storageServer)

-- | Controllers

-- | Clean folder and recreate it, forwarding all exceptions into the
-- request answer
initController :: Handler StorageServerStatus
initController = liftIO (initialize `catch` exceptionWrapper)
  where
    initialize :: IO StorageServerStatus
    initialize = do
      removeDirectoryRecursive baseLocalDir
        `catch` (\e -> putStrLn ("Exception: " ++ show (e :: IOError)))
      createDirectoryIfMissing True baseLocalDir
      StorageServerOk . Size <$> liftIO (getAvailSpace baseLocalDir)

-- | TODO: Add exception handling for edge cases
treeController :: Handler (FileTree ())
treeController = liftIO (traverseDirectory baseLocalDir)

statusController :: Handler StorageServerStatus
statusController =
  StorageServerOk . Size
    <$> liftIO (getAvailSpace baseLocalDir)

fileCreateController :: String -> Handler (FileStatus ())
fileCreateController filepath =
  liftIO (writeFile (baseLocalDir ++ filepath) "")
    >> return (FileOk ())

fileReadController :: Tagged Handler Application
fileReadController = serveDirectoryFileServer baseLocalDir

fileWriteController :: MultipartData Tmp -> Handler (FileStatus ())
fileWriteController query = case extractWriteData query of
  (Left err) -> return (FileError (CustomFileError (T.pack err)))
  (Right (filepath, filedata)) -> do
    liftIO $ copyFile (baseLocalDir ++ fdPayload filedata) filepath
    return (FileOk ())

fileDeleteController :: String -> Handler (FileStatus ())
fileDeleteController filepath =
  liftIO (removeFile (baseLocalDir ++ filepath))
    >> return (FileOk ())

-- | TODO: implement
fileCopyController :: SourceDest -> Handler (FileStatus ())
fileCopyController _ = throwError (ServerError 501 "Sorry, not yet(" mempty [])

fileMoveController :: SourceDest -> Handler (FileStatus ())
fileMoveController _ = throwError (ServerError 501 "Sorry, not yet(" mempty [])

fileLoadController :: LoadFile -> Handler (FileStatus ())
fileLoadController _ = return (FileOk ())

dirCreateController :: DirPath -> Handler (DirStatus ())
dirCreateController (DirPath dirpath) =
  liftIO (createDirectoryIfMissing True (baseLocalDir ++ dirpath))
    >> return (DirOk ())

dirDeleteController :: DirPath -> Handler (DirStatus ())
dirDeleteController (DirPath dirpath) =
  liftIO (removeDirectoryRecursive (baseLocalDir ++ dirpath))
    >> return (DirOk ())

-- | Helpers

-- | Extract useful data from Multipart
extractWriteData :: MultipartData Tmp -> Either String (String, FileData Tmp)
extractWriteData multidata = do
  filepath <- lookupInput "filepath" multidata
  filedata <- lookupFile "payload" multidata
  return (T.unpack filepath, filedata)

-- Wrap `IOException` into request return type
exceptionWrapper :: IOError -> IO StorageServerStatus
exceptionWrapper e =
  return $
    StorageServerError $
      T.pack ("Exception: " ++ show (e :: IOError))

-- | 'dirPath' MUST be valid path to directory, **not** file
traverseDirectory :: FilePath -> IO (FileTree FileName)
traverseDirectory dirPath = do
  anchoredTree <- build dirPath
  let _ :/ tree = anchoredTree
  return (dirTreeToFileTree tree)

isDirectory :: DirTree a -> Bool
isDirectory (Dir _ _) = True
isDirectory _ = False

-- | TODO: Refactor something, could fail at any point. risky.
dirTreeToFileTree :: DirTree a -> FileTree FileName
dirTreeToFileTree (Dir _ content) = FileTree dirs (Map.fromList (zip files files))
  where
    files = map getFileName $ filter (not . isDirectory) content
    dirsonly = filter isDirectory content
    dirs = Map.fromList (zip (map getDirName dirsonly) (map dirTreeToFileTree dirsonly))
    getFileName :: DirTree a -> FileName
    getFileName (File name _) = FileName name
    getFileName (Failed name _) = FileName name
    getDirName :: DirTree a -> DirName
    getDirName (Dir name _) = DirName name
dirTreeToFileTree (File _ _) = error "Single 'File' is unpresentable in 'FileTree'"
dirTreeToFileTree _ = error "Errors is unpresentable in 'FileTree'"
