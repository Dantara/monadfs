{-# LANGUAGE OverloadedStrings #-}

module NameServer.Server.Controllers.Dir where

import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Control.Monad.STM
import           MonadFS.API.Types
import           NameServer.Client
import           NameServer.Internals
import           NameServer.Server.Helpers
import           NameServer.Server.Models.Dir
import           NameServer.Types
import           Servant.Client


dirCreateController :: DirPath -> AppM (DirStatus ())
dirCreateController path = do
  mng <- asks globalManager
  n <- asks amountOfReplicas
  servers <- asks avaliableSSs
  mTree <- asks fileTree
  tree <- liftIO $ readTVarIO mTree

  excludeNotAvaliableSSs

  addrs <- liftIO $ findNBestAddresses n servers

  handleServersAmount (length addrs) n
    $ either
      (pure . DirError)
      (\t -> updateTree mTree t >> runServerReqs mng addrs >> pure (DirOk ()))
      (createDir path tree)
    where
      runServerReqs mng addrs = liftIO
        $ mapM_ (runClientM (dirCreateClient path)
                            . mkClientEnv mng
                            . addrToBaseUrl) addrs

      updateTree mT t = liftIO $ atomically $ writeTVar mT t

      handleServersAmount servers replicas f
        | servers < replicas = pure
          $ DirError
          $ SystemDirError
          $ CustomSystemError "System does not have enought storage servers"
        | otherwise = f


dirDeleteController :: DirPath -> AppM (DirStatus ())
dirDeleteController path = do
  mng <- asks globalManager
  mTree <- asks fileTree
  tree <- liftIO $ readTVarIO mTree

  case deleteDir path tree of
    Right (addrs, t) -> do
      liftIO $ atomically $ writeTVar mTree t
      runServerReqs mng addrs
      pure $ DirOk ()

    Left e ->
      pure $ DirError e

    where
      runServerReqs mng addrs = liftIO
        $ mapM_ (runClientM (dirDeleteClient path)
                            . mkClientEnv mng
                            . addrToBaseUrl) addrs


dirInfoController :: DirPath -> AppM (DirStatus DirInfo)
dirInfoController path = do
  mTree <- asks fileTree
  tree <- liftIO $ readTVarIO mTree

  pure
    $ either DirError DirOk
    $ getDirInfo path tree


dirExistsController :: DirPath -> AppM (DirStatus ())
dirExistsController path = do
  mTree <- asks fileTree
  tree <- liftIO $ readTVarIO mTree

  pure
    $ either DirError (const $ DirOk ())
    $ getDirInfo path tree
