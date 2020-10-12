{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Client.Commands
import Client.Parse
import Client.Types
import Control.Monad.Reader
import Control.Monad.State (get)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client
import System.Console.Haskeline
import System.Environment (getArgs)
import System.Process (system)

main :: IO ()
main = run

evalCommand :: InputCommand -> InputT (CLIClient Environment String) ()
evalCommand ExitCmd = return ()
evalCommand SkipCmd = return ()
evalCommand (EchoCmd str) = outputStrLn str
evalCommand (CowSayCmd str) = void $ liftIO (system ("cowsay " ++ str))
evalCommand MyIpCmd = getMyIPCommand
evalCommand HelpCmd = outputStr helpList
evalCommand InitCmd = initCommand
evalCommand PWDCmd = lift get >>= outputStrLn
evalCommand (TouchCmd fl) = lift get >>= \pref -> touchCommand (pref ++ fl)
evalCommand (GetCmd remote loc) = lift get >>= \pref -> getCommand (pref ++ remote) loc
evalCommand (PutCmd loc remote) = lift get >>= \pref -> putCommand loc (pref ++ remote)
evalCommand (RemoveCmd fl) = lift get >>= \pref -> removeCommand (pref ++ fl)
evalCommand (FileInfoCmd fl) = lift get >>= \pref -> fileInfoCommand (pref ++ fl)
evalCommand (CopyCmd src dst) = copyCommand src dst
evalCommand (MoveCmd src dst) = moveCommand src dst
evalCommand (MakeDirCmd dr) = lift get >>= \pref -> makeDirCommand (pref ++ dr)
evalCommand (RemoveDirCmd dr) = lift get >>= \pref -> removeDirCommand (pref ++ dr)
evalCommand (LsDirCmd dr) = lift get >>= \pref -> dirInfoCommand (pref ++ dr)
evalCommand (DirInfoCmd dr) = dirInfoCommand dr
evalCommand (ChangeDirCmd path) = lift get >>= \pref -> changeDirCommand (pref ++ path)

mainLoop :: InputT (CLIClient Environment String) ()
mainLoop = do
  input <- lift get >>= (\dir -> getInputLine (dir ++ " >>= "))
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
  args <- getArgs
  if length args /= 2
    then putStrLn "Usage: client <nameserver address> <port>"
    else
      runCLIClient
        ( Environment
            mngr
            (BaseUrl Http (args !! 0) (read $ args !! 1) "")
        )
        "/"
        (runInputT defaultSettings welcome)
