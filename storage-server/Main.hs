{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import MonadFS.API.StorageServer (StorageServerAPI)
import MonadFS.API.Types
import MonadFS.FileTree (FileTree, dirTreeToFileTree)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Client
import Servant.Multipart
import System.Directory
import System.Directory.Tree (AnchoredDirTree ((:/)), build)
import System.DiskSpace (getAvailSpace)
import System.Environment (getArgs)
import Text.Read (readMaybe)

baseLocalDir :: FilePath
baseLocalDir = "/monadfs/storage"

storageServerDefaultPort :: Int
storageServerDefaultPort = 4000

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

-- | Download file from another storage
type DownloadApi = "file" :> "read" :> Capture "" String :> Get '[OctetStream] BS.ByteString

downloadapi :: Proxy DownloadApi
downloadapi = Proxy

downloadClient :: String -> ClientM BS.ByteString
downloadClient = client downloadapi

parsePort :: [String] -> Int
parsePort [port] = maybe storageServerDefaultPort id (readMaybe port)
parsePort _ = storageServerDefaultPort

main :: IO ()
main = do
  port <- parsePort <$> getArgs
  putStrLn $ "[+] Starting Storage Server on " <> show port <> " port."
  run port (serve storageServerApi storageServer)

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

fileCopyController :: SourceDest -> Handler (FileStatus ())
fileCopyController (SourceDest src dst) =
  liftIO (copyFileWithMetadata (baseLocalDir ++ src) (baseLocalDir ++ dst))
    >> return (FileOk ())

fileMoveController :: SourceDest -> Handler (FileStatus ())
fileMoveController struct@(SourceDest src _dst) =
  fileCopyController struct
    >> fileDeleteController src

fileLoadController :: LoadFile -> Handler (FileStatus ())
fileLoadController (LoadFile filepath (ServerAddr addr port)) = do
  mngr <- liftIO $ newManager defaultManagerSettings
  let url = BaseUrl Http addr port ""
  eitherContent <-
    liftIO $
      runClientM
        (downloadClient (drop 1 filepath))
        (mkClientEnv mngr url)
  case eitherContent of
    (Left err) -> return (FileError $ CustomFileError (T.pack $ show err))
    (Right content) ->
      liftIO (BS.writeFile (baseLocalDir ++ filepath) content)
        >> return (FileOk ())

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
traverseDirectory :: FilePath -> IO (FileTree ())
traverseDirectory dirPath = do
  anchoredTree <- build dirPath
  let _ :/ tree = const () <$> anchoredTree
  return (dirTreeToFileTree tree)
