module NameServer.Server where

import           Control.Monad.Reader
import           MonadFS.API.NameServer
import           NameServer.Server.Controllers
import           NameServer.Types
import           Network.Wai.Handler.Warp      (run)
import           Servant


nameServerApi :: Proxy NameServerAPI
nameServerApi = Proxy


nameServerApp :: AppState -> Application
nameServerApp state = serve nameServerApi
  $ hoistServer nameServerApi toHandler controllers
  where
    toHandler x = runReaderT (runAppM x) state


runServer :: Int -> AppState -> IO ()
runServer p state = run p (nameServerApp state)
