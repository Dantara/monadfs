module NameServer.Server.Controllers.Root where

import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Control.Monad.STM
import           MonadFS.API.Types
import           NameServer.Client.Requests
import           NameServer.Server.Models.Root
import           NameServer.Types

initController :: AppM SystemStatus
initController = do
  ssAvbl <- asks avaliableSSs
  ssList <- asks ssAddrs
  mng <- asks globalManager
  tTree <- asks fileTree

  let proceed = proceedRequestM mng

  ssAvbl' <- liftIO $ proceed $ lookupSSs ssList
  liftIO $ atomically $ writeTVar ssAvbl ssAvbl'
  liftIO $ atomically $ writeTVar tTree initVFS
  liftIO $ proceed (initializeSSs $ ssAddr <$> ssAvbl')

  case length ssAvbl' of
    0 ->
      pure $ SystemError NoStorageServersAvaliable
    _ -> do
      rs <- asks amountOfReplicas
      let totalSize = sum $ (\(Size s) -> s) . ssSize <$> ssAvbl'
      pure $ SystemOk $ Size $ totalSize `div` fromIntegral rs
