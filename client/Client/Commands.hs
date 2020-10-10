{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Client.Commands where

import Client.Types
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import MonadFS.API.NameServer
import MonadFS.API.StorageServer
import MonadFS.API.Types
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.Client
import Servant.Multipart
import System.Console.Haskeline

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
fileWriteClient :: (BL.ByteString, MultipartData Tmp) -> ClientM (FileStatus ())
-- fileReadClient :: BS.ByteString -> ClientM Response
initClient
  :<|> ( fileCreateClient :<|> _
           :<|> _
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
  :<|> ( _ :<|> fileReadClient :<|> fileWriteClient
           :<|> _
           :<|> _
           :<|> _
         )
  :<|> (_ :<|> _) = client storageServerApi

getMyIPCommand :: InputT (CLIClient Environment String) ()
getMyIPCommand =
  lift (asks envManager)
    >>= (\mngr -> liftIO (runClientM getIP (mkClientEnv mngr (BaseUrl Http "ifconfig.me" 80 ""))))
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

touchCommand :: String -> InputT (CLIClient Environment String) ()
touchCommand filepath = do
  env <- lift (asks mkNameEnv)
  ans <- liftIO (runClientM (fileCreateClient filepath) env)
  outputStrLn (show ans)

getCommand :: String -> InputT (CLIClient Environment String) ()
getCommand filepath = outputStrLn "Not implemented yet("

putCommand :: String -> InputT (CLIClient Environment String) ()
putCommand filepath = outputStrLn "Not implemented yet("

-- putCommand filepath = do
--   env <- lift (asks mkNameEnv)
--   boundary <- liftIO genBoundary
--   let textPath = T.pack filepath
--   let reqBody = MultipartData [Input "filepath" textPath] [FileData "payload" textPath "application/octet-stream" filepath]
--   ans <- liftIO (runClientM (fileWriteClient (boundary, reqBody)) env)
--   outputStrLn (show ans)

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
