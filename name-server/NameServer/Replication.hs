module NameServer.Replication where

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.STM
import           NameServer.Client.Requests
import           NameServer.Types

runReplication :: AppState -> Int -> IO ()
runReplication state sec = forever $ do
  let mng = globalManager state
  let addrs = ssAddrs state
  let tServers = avaliableSSs state
  servers <- proceedRequestM mng $ lookupSSs addrs
  atomically $ writeTVar tServers servers
  threadDelay (sec * 1000000)
