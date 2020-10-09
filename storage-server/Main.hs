module Main where

import qualified Data.Map.Strict as Map (empty)
import MonadFS.API.StorageServer (StorageServerAPI)
import MonadFS.API.Types
import MonadFS.FileTree (FileTree (FileTree))
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Multipart (MultipartData, Tmp)

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
         )
    :<|> ( dirCreateController :<|> dirDeleteController
         )

main :: IO ()
main = do
  putStrLn $ "[+] Starting Storage Server on " <> show storageServerPort <> " port."
  run storageServerPort (serve storageServerApi storageServer)

-- | Controllers
initController :: Handler StorageServerStatus
initController = return (StorageServerOk (Size 666))

treeController :: Handler (FileTree FileName)
treeController = return (FileTree Map.empty Map.empty)

statusController :: Handler StorageServerStatus
statusController = return (StorageServerOk (Size 666))

fileCreateController :: String -> Handler (FileStatus ())
fileCreateController _ = return (FileOk ())

fileReadController :: Tagged Handler Application
fileReadController = serveDirectoryFileServer "/var/www/html"

fileWriteController :: MultipartData Tmp -> Handler (FileStatus ())
fileWriteController _ = return (FileOk ())

fileDeleteController :: String -> Handler (FileStatus ())
fileDeleteController _ = return (FileOk ())

fileCopyController :: String -> Handler (FileStatus ())
fileCopyController _ = return (FileOk ())

fileMoveController :: String -> Handler (FileStatus ())
fileMoveController _ = return (FileOk ())

dirCreateController :: DirPath -> Handler (DirStatus ())
dirCreateController _ = return (DirOk ())

dirDeleteController :: DirPath -> Handler (DirStatus ())
dirDeleteController _ = return (DirOk ())
