{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Concurrent.STM.TVar
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.List
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (isJust)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           MonadFS.API.NameServer
import           MonadFS.API.StorageServer
import           MonadFS.API.Types
import           MonadFS.FileTree
import           Network.HTTP.Client         (Manager, defaultManagerSettings,
                                              newManager)
import           Network.Wai.Handler.Warp    (run)
import           Servant
import           Servant.Client

nameServerPort :: Int
nameServerPort = 4000

newtype AppM a = AppM { runAppM :: ReaderT AppState Handler a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader AppState
           , MonadIO
           )

data AppState = AppState {
    fileTree         :: TVar VFS
  , avaliableSSs     :: TVar [StorageServer]
  , ssAddrs          :: [ServerAddr]
  , globalManager    :: Manager
  , amountOfReplicas :: Int
  }

data StorageServer = StorageServer {
    ssAddr :: ServerAddr
  , ssSize :: Size
  } deriving (Eq, Ord)

newtype RequestM a = RequestM { runRequestM :: ReaderT Manager IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Manager
           , MonadIO
           )


initState :: IO AppState
initState = do
  ft <- newTVarIO initVFS
  aSSs <- newTVarIO []
  mng <- newManager defaultManagerSettings
  pure $ AppState ft aSSs addrs mng replicas
  where
    replicas = 2
    addrs = [
        ServerAddr "127.0.0.1" 3000
      , ServerAddr "127.0.0.1" 3040
      , ServerAddr "127.0.0.1" 3080
      ]

main :: IO ()
main = do
  putStrLn $ "[+] Starting Name Server on " <> show nameServerPort <> " port."
  s <- initState
  run nameServerPort $ nameServerApp s


nameServerApp :: AppState -> Application
nameServerApp state = serve nameServerApi
  $ hoistServer nameServerApi toHandler servantServer
  where
    toHandler x = runReaderT (runAppM x) state


nameServerApi :: Proxy NameServerAPI
nameServerApi = Proxy

storageServerApi :: Proxy StorageServerAPI
storageServerApi = Proxy


servantServer :: ServerT NameServerAPI AppM
servantServer = initController
  :<|> (fileCreateController :<|> fileReadController
       :<|> fileWriteController :<|> fileDeleteController
       :<|> fileInfoController :<|> fileCopyController
       :<|> fileMoveController)
  :<|> (dirCreateController :<|> dirDeleteController
       :<|> dirInfoController :<|> dirExistsController)

-- | Controllers

initController :: AppM SystemStatus
initController = do
  ssAvbl <- asks avaliableSSs
  ssList <- asks ssAddrs
  mng <- asks globalManager

  let proceed = proceedRequestM mng

  ssAvbl' <- liftIO $ proceed $ lookupSSs ssList
  liftIO $ atomically $ writeTVar ssAvbl ssAvbl'
  liftIO $ proceed (initializeSSs $ ssAddr <$> ssAvbl')

  case length ssAvbl' of
    0 ->
      pure $ SystemError NoStorageServersAvaliable
    _ -> do
      rs <- asks amountOfReplicas
      let totalSize = sum $ (\(Size s) -> s) . ssSize <$> ssAvbl'
      pure $ SystemOk $ Size $ totalSize `div` fromIntegral rs


fileCreateController :: FilePath -> AppM (FileStatus ())
fileCreateController path = do
  mng <- asks globalManager
  n <- asks amountOfReplicas
  servers <- asks avaliableSSs
  mTree <- asks fileTree
  tree <- liftIO $ readTVarIO mTree

  excludeNotAvaliableSSs

  addrs <- liftIO $ (take n
                     . map (\(StorageServer x _) -> x)
                     . reverse
                     . sortBy (\(StorageServer _ l) (StorageServer _ r) -> compare l r)
                    ) <$> (readTVarIO servers)

  handleServersAmount (length addrs) n
    $ either
      (\e -> pure $ FileError e)
      (\t -> updateTree mTree t >> runServerReqs mng addrs >> (pure $ FileOk ()))
      (addFileToTree path addrs tree)

    where
      runServerReqs mng addrs = liftIO
        $ mapM_ (runClientM (fileCreateClient path)
                            . mkClientEnv mng
                            . addrToBaseUrl) addrs

      updateTree mT t = liftIO $ atomically $ writeTVar mT t

      handleServersAmount servers replicas f
        | servers < replicas = pure
          $ FileError
          $ SystemFileError
          $ CustomSystemError "System does not have enought storage servers"
        | otherwise = f




fileReadController :: FilePath -> AppM (FileStatus ServerAddr)
fileReadController = undefined

fileWriteController :: NewFile -> AppM (FileStatus [ServerAddr])
fileWriteController = undefined

fileDeleteController :: FilePath -> AppM (FileStatus ())
fileDeleteController = undefined

fileInfoController :: FilePath -> AppM (FileStatus FileInfo)
fileInfoController = undefined

fileCopyController :: FilePath -> AppM (FileStatus ())
fileCopyController = undefined

fileMoveController :: FilePath -> AppM (FileStatus ())
fileMoveController = undefined


dirCreateController :: DirPath -> AppM (DirStatus ())
dirCreateController = undefined

dirDeleteController :: DirPath -> AppM (DirStatus ())
dirDeleteController = undefined

dirInfoController :: DirPath -> AppM (DirStatus DirInfo)
dirInfoController = undefined

dirExistsController :: DirPath -> AppM (DirStatus ())
dirExistsController = undefined

-- | Clients

initClient :<|> treeClient :<|> statusClient
  :<|> (fileCreateClient :<|> fileReadClient :<|> fileWriteFile
       :<|> fileDeleteClient :<|> fileCopyClient :<|> fileMoveClient)
  :<|> (dirCreateClient :<|> dirDeleteClient) = client storageServerApi

-- | Helpers

checkSS :: ServerAddr -> RequestM (Maybe StorageServer)
checkSS addr = do
  mng <- ask
  res <- liftIO
    $ runClientM statusClient
    $ mkClientEnv mng
    $ addrToBaseUrl addr
  case res of
    Right (StorageServerOk s) ->
      pure $ Just $ StorageServer addr s
    _ ->
      pure Nothing


lookupSSs :: [ServerAddr] -> RequestM [StorageServer]
lookupSSs addrs = mapM checkSS addrs
  >>= (pure . map (\(Just x) -> x) . filter isJust)


excludeNotAvaliableSSs :: AppM ()
excludeNotAvaliableSSs = do
  ssAvbl <- asks avaliableSSs
  ssList <- (fmap ssAddr) <$> (liftIO $ readTVarIO ssAvbl)
  mng <- asks globalManager

  ssAvbl' <- liftIO $ proceedRequestM mng $ lookupSSs ssList
  liftIO $ atomically $ writeTVar ssAvbl ssAvbl'

initVFS :: VFS
initVFS = FileTree Map.empty Map.empty

initializeSSs :: [ServerAddr] -> RequestM ()
initializeSSs = mapM_ initSS
  where
    initSS addr = do
      mng <- ask
      liftIO
        $ runClientM initClient
        $ mkClientEnv mng
        $ addrToBaseUrl addr


addrToBaseUrl :: ServerAddr -> BaseUrl
addrToBaseUrl (ServerAddr url port) = BaseUrl Http url port ""

proceedRequestM :: Manager -> RequestM a -> IO a
proceedRequestM mng req = runReaderT (runRequestM req) mng

addFileToTree :: FilePath -> [ServerAddr] -> VFS -> Either FileError VFS
addFileToTree path addrs tree
  | path == "" = Left IncorrectFilePath
  | name == "" = Left $ CustomFileError "Empty file name"
  | Map.member fileName' $ files tree = Left FileExists
  | null tail' = Right
    $ tree { files = Map.insert fileName' fileNode' (files tree) }
  | otherwise = case Map.lookup dirName' dirs of
      Just subTree ->
          either
            (\x -> Left x)
            (\t -> Right $ tree { directories = Map.update (\_ -> Just t) dirName' dirs })
            (addFileToTree tail' addrs subTree)
      Nothing ->
        Left IncorrectFilePath
    where
      name = takeWhile (== '/') path
      tail' = dropWhile (== '/') path
      fileName' = FileName name
      dirName' = DirName name
      fileNode' = FileNode (FileName name) (FileInfo (Size 0) addrs)
      dirs = directories tree
