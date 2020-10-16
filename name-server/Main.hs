{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Data.Aeson
import           Data.ByteString.Lazy          as BS (readFile)
import           NameServer.Cache
import           NameServer.Replication
import           NameServer.Server
import           NameServer.Server.Models.Root
import           NameServer.Types
import           Network.HTTP.Client           (defaultManagerSettings,
                                                newManager)
import           System.Directory              (doesFileExist)


configFileName :: FilePath
configFileName = "name-server.conf"


readConfig :: IO (Maybe Config)
readConfig = do
  isExist <- doesFileExist configFileName
  if isExist then
    decode <$> BS.readFile configFileName
  else pure Nothing


initState :: Config -> IO AppState
initState conf = do
  ft <- newTVarIO initVFS
  aSSs <- newTVarIO []
  mng <- newManager defaultManagerSettings
  pure $ AppState ft aSSs addrs mng replicas
  where
    replicas = replicasAmount conf
    addrs = storageServersAddrs conf


main :: IO ()
main = do
  putStrLn "[+] Reading configuration file."
  conf <- readConfig

  case conf of
    Nothing ->
      putStrLn "[#] Not able read configuration file."
    Just conf' -> do
      s <- initState conf'

      readCache (fileTreeCacheFile conf') (fileTree s)

      _ <- forkIO
        $ writeCache (fileTreeCacheFile conf') (fileTreeCachePeriod conf') (fileTree s)
      _ <- forkIO
        $ runReplication s (avaliablityCheckingPeriod conf')

      putStrLn $ "[+] Starting Name Server on " <> show (port conf') <> " port."

      runServer (port conf') s
