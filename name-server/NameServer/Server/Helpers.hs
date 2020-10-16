module NameServer.Server.Helpers where

import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.List
import           MonadFS.API.Types
import           NameServer.Client.Requests
import           NameServer.Types


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

