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

proceedRequestM :: Manager -> RequestM a -> IO a
proceedRequestM mng req = runReaderT (runRequestM req) mng


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
  status <- newFileHelper (NewFile path (Size 0))

  pure $ case status of
           (FileOk _)    -> FileOk ()
           (FileError e) -> FileError e



fileReadController :: FilePath -> AppM (FileStatus ServerAddr)
fileReadController path = do
  mng <- asks globalManager
  mTree <- asks fileTree
  tree <- liftIO $ readTVarIO mTree

  case findFileNode path tree of
    Right (FileNode _ (FileInfo _ servers)) -> do
      addrs <- liftIO $ fmap ssAddr
               <$> proceedRequestM mng (lookupSSs servers)

      pure $ if null addrs then FileError $ SystemFileError NoStorageServersAvaliable
                           else FileOk $ head addrs

    Left e ->
      pure $ FileError e

fileWriteController :: NewFile -> AppM (FileStatus [ServerAddr])
fileWriteController = newFileHelper

fileDeleteController :: FilePath -> AppM (FileStatus ())
fileDeleteController path = do
  mng <- asks globalManager
  mTree <- asks fileTree
  tree <- liftIO $ readTVarIO mTree

  case deleteFileNode path tree of
    Right (FileNode _ (FileInfo _ addrs), t) -> do
      liftIO $ atomically $ writeTVar mTree t
      runServerReqs mng addrs
      pure $ FileOk ()

    Left e ->
      pure $ FileError e

    where
      runServerReqs mng addrs = liftIO
        $ mapM_ (runClientM (fileDeleteClient path)
                            . mkClientEnv mng
                            . addrToBaseUrl) addrs

fileInfoController :: FilePath -> AppM (FileStatus FileInfo)
fileInfoController path = do
  mTree <- asks fileTree
  tree <- liftIO $ readTVarIO mTree

  pure
    $ either FileError (FileOk . fileInfo)
    $ findFileNode path tree


fileCopyController :: FilePath -> AppM (FileStatus ())
fileCopyController path = do
  mng <- asks globalManager
  n <- asks amountOfReplicas
  servers <- asks avaliableSSs
  mTree <- asks fileTree
  tree <- liftIO $ readTVarIO mTree

  addrs <- liftIO $ findNBestAddresses n servers

  handleServersAmount (length addrs) n
    $ either
      (pure . FileError)

      (\t -> updateTree mTree t >> runServerReqs mng addrs >> pure (FileOk ()))

      (deleteFileNode path tree
       >>= (\(FileNode _ (FileInfo s _), tree') ->
              addFileToTree (NewFile path s) addrs tree'))

    where
      runServerReqs mng addrs = liftIO
        $ mapM_ (runClientM (fileMoveClient path)
                            . mkClientEnv mng
                            . addrToBaseUrl) addrs

      updateTree mT t = liftIO $ atomically $ writeTVar mT t

      handleServersAmount servers replicas f
        | servers < replicas = pure
          $ FileError
          $ SystemFileError
          $ CustomSystemError "System does not have enought storage servers"
        | otherwise = f

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

initClient :: ClientM StorageServerStatus
treeClient :: ClientM (FileTree FileName)
statusClient :: ClientM StorageServerStatus
fileCreateClient :: FilePath -> ClientM (FileStatus ())
fileDeleteClient :: FilePath -> ClientM (FileStatus ())
fileCopyClient :: FilePath -> ClientM (FileStatus ())
fileMoveClient :: FilePath -> ClientM (FileStatus ())
dirCreateClient :: DirPath -> ClientM (DirStatus ())
dirDeleteClient :: DirPath -> ClientM (DirStatus ())


initClient :<|> treeClient :<|> statusClient
  :<|> (fileCreateClient :<|> fileReadClient :<|> fileWriteFile
       :<|> fileDeleteClient :<|> fileCopyClient :<|> fileMoveClient)
  :<|> (dirCreateClient :<|> dirDeleteClient) = client storageServerApi


-- | Controller Helpers


newFileHelper :: NewFile -> AppM (FileStatus [ServerAddr])
newFileHelper newF@(NewFile path _) = do
  mng <- asks globalManager
  n <- asks amountOfReplicas
  servers <- asks avaliableSSs
  mTree <- asks fileTree
  tree <- liftIO $ readTVarIO mTree

  excludeNotAvaliableSSs

  addrs <- liftIO $ findNBestAddresses n servers

  handleServersAmount (length addrs) n
    $ either
      (pure . FileError)
      (\t -> updateTree mTree t >> runServerReqs mng addrs >> pure (FileOk addrs))
      (addFileToTree newF addrs tree)

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


excludeNotAvaliableSSs :: AppM ()
excludeNotAvaliableSSs = do
  ssAvbl <- asks avaliableSSs
  ssList <- fmap ssAddr <$> liftIO (readTVarIO ssAvbl)
  mng <- asks globalManager

  ssAvbl' <- liftIO $ proceedRequestM mng $ lookupSSs ssList
  liftIO $ atomically $ writeTVar ssAvbl ssAvbl'


findNBestAddresses :: Int -> TVar [StorageServer] -> IO [ServerAddr]
findNBestAddresses n servers = take n
                    . map (\(StorageServer x _) -> x)
                    . sortBy (\(StorageServer _ l) (StorageServer _ r) -> compare r l)
                    <$> readTVarIO servers

-- | Client Requests


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


initializeSSs :: [ServerAddr] -> RequestM ()
initializeSSs = mapM_ initSS
  where
    initSS addr = do
      mng <- ask
      liftIO
        $ runClientM initClient
        $ mkClientEnv mng
        $ addrToBaseUrl addr



-- | Pure business logic


initVFS :: VFS
initVFS = FileTree Map.empty Map.empty

addrToBaseUrl :: ServerAddr -> BaseUrl
addrToBaseUrl (ServerAddr url port) = BaseUrl Http url port ""

addFileToTree :: NewFile -> [ServerAddr] -> VFS -> Either FileError VFS
addFileToTree (NewFile path size) addrs tree
  | null path = Left IncorrectFilePath
  | null name = Left $ CustomFileError "Empty file name"
  | Map.member fileName' $ files tree = Left FileExists
  | null tail' = Right
    $ tree { files = Map.insert fileName' fileNode' (files tree) }
  | otherwise = case Map.lookup dirName' dirs of
      Just subTree ->
          either
            Left
            (\t -> Right $ tree { directories = Map.update (\_ -> Just t) dirName' dirs })
            (addFileToTree (NewFile tail' size) addrs subTree)
      Nothing ->
        Left IncorrectFilePath
    where
      name = takeWhile (== '/') path
      tail' = dropWhile (== '/') path
      fileName' = FileName name
      dirName' = DirName name
      fileNode' = FileNode (FileName name) (FileInfo size addrs)
      dirs = directories tree


findFileNode :: FilePath -> VFS -> Either FileError FileNode
findFileNode path tree
  | null path = Left IncorrectFilePath
  | null name = Left $ CustomFileError "Empty file name"
  | null tail' = maybe
    (Left FileDoesNotExist)
    Right
    (Map.lookup fileName' (files tree))
  | null dirs = Left FileDoesNotExist
  | otherwise = maybe
    (Left IncorrectFilePath)
    (findFileNode tail')
    (Map.lookup dirName' dirs)
    where
      name = takeWhile (== '/') path
      tail' = dropWhile (== '/') path
      fileName' = FileName name
      dirName' = DirName name
      dirs = directories tree


deleteFileNode :: FilePath -> VFS -> Either FileError (FileNode, VFS)
deleteFileNode path tree
  | null path = Left IncorrectFilePath
  | Map.member fileName' $ files tree = Left FileExists
  | null tail' = maybe
    (Left FileDoesNotExist)
    (\x -> Right (x, tree { files = Map.delete fileName' fs }))
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
      name = takeWhile (== '/') path
      tail' = dropWhile (== '/') path
      fileName' = FileName name
      dirName' = DirName name
      dirs = directories tree
      fs = files tree
