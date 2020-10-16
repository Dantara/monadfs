{-# LANGUAGE OverloadedStrings #-}

module NameServer.Server.Controllers.File where

import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Control.Monad.STM
import           MonadFS.API.Types
import           MonadFS.FileTree
import           NameServer.Client
import           NameServer.Client.Requests
import           NameServer.Internals
import           NameServer.Server.Helpers
import           NameServer.Server.Models.File
import           NameServer.Types
import           Servant.Client


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


fileCopyController :: SourceDest -> AppM (FileStatus ())
fileCopyController sd@(SourceDest source dest) = do
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

      (findFileNode source tree
       >>= (\(FileNode _ (FileInfo s _)) ->
              addFileToTree (NewFile dest s) addrs tree))

    where
      runServerReqs mng addrs = liftIO
        $ mapM_ (runClientM (fileMoveClient sd)
                            . mkClientEnv mng
                            . addrToBaseUrl) addrs

      updateTree mT t = liftIO $ atomically $ writeTVar mT t

      handleServersAmount servers replicas f
        | servers < replicas = pure
          $ FileError
          $ SystemFileError
          $ CustomSystemError "System does not have enought storage servers"
        | otherwise = f


fileMoveController :: SourceDest -> AppM (FileStatus ())
fileMoveController sd@(SourceDest source dest) = do
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

      (deleteFileNode source tree
       >>= (\(FileNode _ (FileInfo s _), tree') ->
              addFileToTree (NewFile dest s) addrs tree'))

    where
      runServerReqs mng addrs = liftIO
        $ mapM_ (runClientM (fileMoveClient sd)
                            . mkClientEnv mng
                            . addrToBaseUrl) addrs

      updateTree mT t = liftIO $ atomically $ writeTVar mT t

      handleServersAmount servers replicas f
        | servers < replicas = pure
          $ FileError
          $ SystemFileError
          $ CustomSystemError "System does not have enought storage servers"
        | otherwise = f


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
