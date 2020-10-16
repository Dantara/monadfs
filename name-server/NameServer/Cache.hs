module NameServer.Cache where

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.STM
import           Data.Aeson
import           Data.ByteString.Lazy        as BS (readFile, writeFile)
import           MonadFS.FileTree
import           System.Directory            (doesFileExist)


readCache :: FilePath -> TVar VFS -> IO ()
readCache path tVfs = do
  putStrLn "[+] Checking cached version of VFS."
  isExist <- doesFileExist path

  case isExist of
    True -> do
      putStrLn "[+] name-server.tree file is found."
      putStrLn "[+] Restoring the cache."
      cache <- BS.readFile path

      maybe
        (putStrLn "[#] Cache is malformed.")
        (\t -> (atomically $ writeTVar tVfs t)
          >> putStrLn "[+] Cache is restored.")
        (decode cache)

    False ->
      putStrLn "[+] Cache is not found. Using empty state."


writeCache :: FilePath -> Int -> TVar VFS -> IO ()
writeCache path sec tVfs = forever $ do
  vfs <- readTVarIO tVfs
  BS.writeFile path (encode vfs)
  threadDelay (sec * 1000000)
