{-# LANGUAGE FlexibleContexts #-}

module NameServer.Client.Requests where

import           Control.Monad.Reader
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           MonadFS.API.Types
import           MonadFS.FileTree
import           NameServer.Client
import           NameServer.Internals
import           NameServer.Types
import           Servant.Client


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


fetchStorageTrees :: [ServerAddr] -> RequestM [StorageTree]
fetchStorageTrees = mapM fetchTree
  where
    fetchTree addr = do
      mng <- ask
      fmap (either (const emptyTree) id)
        <$> liftIO
        $ runClientM treeClient
        $ mkClientEnv mng
        $ addrToBaseUrl addr

    emptyTree = FileTree Map.empty Map.empty


runFixCommands :: [(ServerAddr, FixCommand)] -> RequestM ()
runFixCommands = mapM_ runFixCommand
  where
    runFixCommand (addr, CreateDir path) = sendReq addr
      $ dirCreateClient path
    runFixCommand (addr, RemoveDir path) = sendReq addr
      $ dirDeleteClient path
    runFixCommand (addr, RemoveFile path) = sendReq addr
      $ fileDeleteClient path
    runFixCommand (addr, LoadMissingFile lf) = sendReq addr
      $ fileLoadClient lf

    sendReq addr client' = do
      mng <- ask
      liftIO
        $ void
        $ runClientM client'
        $ mkClientEnv mng
        $ addrToBaseUrl addr
