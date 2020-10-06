{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Client.Parse
import Client.Types
import Control.Monad.Reader
import Data.Proxy (Proxy (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API (Get, PlainText, type (:>))
import Servant.Client
import System.Console.Haskeline
import System.Process (system)

main :: IO ()
main = run

type MyAPI = "ip" :> Get '[PlainText] String

api :: Proxy MyAPI
api = Proxy

getIP :: ClientM String
getIP = client api

myURL :: BaseUrl
myURL = BaseUrl Http "ifconfig.me" 80 ""

evalCommand :: InputCommand -> InputT (CLIClient Environment String) ()
evalCommand ExitCmd = return ()
evalCommand SkipCmd = return ()
evalCommand (EchoCmd str) = outputStrLn str
evalCommand (CowSayCmd str) = void $ liftIO (system ("cowsay " ++ str))
evalCommand MyIpCmd =
  lift ask
    >>= (\(Environment mngr) -> liftIO (runClientM getIP (mkClientEnv mngr myURL)))
    >>= outputStrLn . show
evalCommand HelpCmd = outputStr helpList
evalCommand _ = outputStrLn "Not implemented yet("

mainLoop :: InputT (CLIClient Environment String) ()
mainLoop = do
  input <- getInputLine ">>= "
  case parseCommand input of
    (Left msg) -> outputStrLn ("Wrong command, " ++ show msg) >> mainLoop
    (Right ExitCmd) -> return ()
    (Right cmd) -> evalCommand cmd >> mainLoop

welcome :: InputT (CLIClient Environment String) ()
welcome = do
  outputStrLn "Welcome to DFS client! Type 'help' for help;)"
  mainLoop
  outputStrLn "Goodbye!"

run :: IO ()
run = do
  mngr <- liftIO $ newManager defaultManagerSettings
  runCLIClient (Environment mngr) "/" (runInputT defaultSettings welcome)
