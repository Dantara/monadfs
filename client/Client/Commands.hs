{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Client.Commands where

import Client.Types
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import MonadFS.API.NameServer
import MonadFS.API.StorageServer
import MonadFS.API.Types
import qualified Network.HTTP.Client as Net
import Servant
import Servant.Client
import Servant.Multipart
import System.Console.Haskeline
import System.Directory (getFileSize)

nameServerApi :: Proxy NameServerAPI
nameServerApi = Proxy

storageServerApi :: Proxy StorageServerAPI
storageServerApi = Proxy

initClient :: ClientM SystemStatus
fileCreateClient :: String -> ClientM (FileStatus ())
fileDeleteClient :: [Char] -> ClientM (FileStatus ())
fileInfoClient :: [Char] -> ClientM (FileStatus FileInfo)
fileCopyClient :: SourceDest -> ClientM (FileStatus ())
fileMoveClient :: SourceDest -> ClientM (FileStatus ())
dirCreateClient :: DirPath -> ClientM (DirStatus ())
dirDeleteClient :: DirPath -> ClientM (DirStatus ())
dirInfoClient :: DirPath -> ClientM (DirStatus DirInfo)
dirExistsClient :: DirPath -> ClientM (DirStatus ())
fileReadClient :: [Char] -> ClientM (FileStatus ServerAddr)
fileWriteClient :: NewFile -> ClientM (FileStatus [ServerAddr])
filePutClient :: (BL.ByteString, MultipartData Tmp) -> ClientM (FileStatus ())
initClient
  :<|> ( fileCreateClient :<|> fileReadClient
           :<|> fileWriteClient
           :<|> fileDeleteClient
           :<|> fileInfoClient
           :<|> fileCopyClient
           :<|> fileMoveClient
         )
  :<|> ( dirCreateClient :<|> dirDeleteClient
           :<|> dirInfoClient
           :<|> dirExistsClient
         ) = client nameServerApi

_ :<|> _ :<|> _
  :<|> ( _ :<|> _ :<|> filePutClient
           :<|> _
           :<|> _
           :<|> _
         )
  :<|> (_ :<|> _) = client storageServerApi

-- | Download file from another storage
getMyIPCommand :: InputT (CLIClient Environment String) ()
getMyIPCommand =
  lift (asks envManager)
    >>= ( \mngr ->
            liftIO
              ( runClientM
                  getIP
                  ( mkClientEnv
                      mngr
                      (BaseUrl Http "ifconfig.me" 80 "")
                  )
              )
        )
    >>= outputStrLn . show
  where
    api :: Proxy ("ip" :> Get '[PlainText] String)
    api = Proxy
    getIP :: ClientM String
    getIP = client api

mkNameEnv :: Environment -> ClientEnv
mkNameEnv (Environment mngr base) = mkClientEnv mngr base

initCommand :: InputT (CLIClient Environment String) ()
initCommand = do
  env <- lift (asks mkNameEnv)
  ans <- liftIO (runClientM initClient env)
  outputStrLn (show ans)

touchCommand :: FilePath -> InputT (CLIClient Environment String) ()
touchCommand filepath = do
  env <- lift (asks mkNameEnv)
  ans <- liftIO (runClientM (fileCreateClient filepath) env)
  outputStrLn (show ans)

getCommand :: FilePath -> FilePath -> InputT (CLIClient Environment String) ()
getCommand remotename localname = do
  env <- lift (asks mkNameEnv)
  mngr <- lift (asks envManager)
  ans <- liftIO (runClientM (fileReadClient remotename) env)
  case ans of
    (Right (FileOk (ServerAddr addr port))) -> do
      adr <- Net.parseRequest ("http://" ++ addr ++ ":" ++ show port ++ "/file/read" ++ remotename)
      response <-
        liftIO
          ( Net.httpLbs adr mngr
          )
      liftIO (BL.writeFile localname (Net.responseBody response))
        >> outputStrLn ("File " ++ remotename ++ " saved to " ++ localname)
    err -> outputStrLn (show err)

putCommand :: FilePath -> FilePath -> InputT (CLIClient Environment String) ()
putCommand localname remotename = wrapper `catch` (\e -> outputStrLn ("Something went wrong: " ++ show (e :: SomeException)))
  where
    wrapper = do
      env <- lift (asks mkNameEnv)
      flsize <- liftIO (getFileSize localname)
      ans <-
        liftIO
          ( runClientM
              ( fileWriteClient
                  (NewFile remotename (Size flsize))
              )
              env
          )
      case ans of
        (Right (FileOk servers)) ->
          sequence_ (fmap (uploadFile localname remotename) servers)
        err -> outputStrLn (show err)

uploadFile ::
  FilePath ->
  FilePath ->
  ServerAddr ->
  InputT (CLIClient Environment String) ()
uploadFile localname remotename (ServerAddr addr port) = do
  mngr <- lift (asks envManager)
  let url = BaseUrl Http addr port ""
  boundary <- liftIO genBoundary
  let reqBody =
        MultipartData
          [Input "filepath" (T.pack remotename)]
          [ FileData
              "payload"
              (T.pack localname)
              "application/octet-stream"
              localname
          ]
  ans <-
    liftIO
      ( runClientM
          (filePutClient (boundary, reqBody))
          (mkClientEnv mngr url)
      )
  outputStrLn (show ans)

removeCommand :: String -> InputT (CLIClient Environment String) ()
removeCommand filepath = do
  env <- lift (asks mkNameEnv)
  ans <- liftIO (runClientM (fileDeleteClient filepath) env)
  outputStrLn (show ans)

fileInfoCommand :: String -> InputT (CLIClient Environment String) ()
fileInfoCommand filepath = do
  env <- lift (asks mkNameEnv)
  ans <- liftIO (runClientM (fileInfoClient filepath) env)
  outputStrLn (show ans)

copyCommand :: String -> String -> InputT (CLIClient Environment String) ()
copyCommand source dest = do
  env <- lift (asks mkNameEnv)
  ans <- liftIO (runClientM (fileCopyClient (SourceDest source dest)) env)
  outputStrLn (show ans)

moveCommand :: String -> String -> InputT (CLIClient Environment String) ()
moveCommand source dest = do
  env <- lift (asks mkNameEnv)
  ans <- liftIO (runClientM (fileMoveClient (SourceDest source dest)) env)
  outputStrLn (show ans)

makeDirCommand :: String -> InputT (CLIClient Environment String) ()
makeDirCommand path = do
  env <- lift (asks mkNameEnv)
  ans <- liftIO (runClientM (dirCreateClient (DirPath path)) env)
  outputStrLn (show ans)

removeDirCommand :: String -> InputT (CLIClient Environment String) ()
removeDirCommand path = do
  env <- lift (asks mkNameEnv)
  ans <- liftIO (runClientM (dirDeleteClient (DirPath path)) env)
  outputStrLn (show ans)

dirInfoCommand :: String -> InputT (CLIClient Environment String) ()
dirInfoCommand path = do
  env <- lift (asks mkNameEnv)
  ans <- liftIO (runClientM (dirInfoClient (DirPath path)) env)
  outputStrLn (show ans)

changeDirCommand :: String -> InputT (CLIClient Environment String) ()
changeDirCommand path = do
  env <- lift (asks mkNameEnv)
  ans <- liftIO (runClientM (dirExistsClient (DirPath path)) env)
  case ans of
    (Right (DirOk _)) -> lift (put path)
    _ -> return ()
