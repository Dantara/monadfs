{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Proxy (Proxy (..))
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Servant.API (Get, PlainText, type (:>))
import Servant.Client
import System.Console.Haskeline
import System.Process (system)
import Text.Parsec

main :: IO ()
main = run

type MyAPI = "ip" :> Get '[PlainText] String

api :: Proxy MyAPI
api = Proxy

getIP :: ClientM String
getIP = client api

myURL :: BaseUrl
myURL = BaseUrl Http "ifconfig.me" 80 ""

data MyEnv = MyEnv Manager BaseUrl (InputT IO ())

data InputCommand
  = ExitCmd
  | SkipCmd
  | HiCmd
  | EchoCmd String
  | CowSayCmd String
  | MyIpCmd
  | TouchCmd String
  | MoveCmd String String
  deriving (Show, Eq)

commands :: Parsec String () InputCommand
commands =
  choice
    [ try
        ( string "q" <|> string "quit"
            <|> string "exit"
            <|> string "bye"
        )
        >> return ExitCmd,
      string "hello" >> return HiCmd,
      string "echo" >> spaces >> EchoCmd <$> many anyChar,
      string "cowsay" >> spaces >> CowSayCmd <$> many anyChar,
      string "myip" >> return MyIpCmd,
      string "touch" >> spaces >> TouchCmd <$> pathParse,
      string "mv" >> spaces >> MoveCmd <$> pathParse <*> (spaces >> pathParse)
    ]

pathParse :: Parsec String () String
pathParse = many1 (alphaNum <|> oneOf "/.@-_+")

parseCommand :: Maybe String -> Either ParseError InputCommand
parseCommand Nothing = Right ExitCmd
parseCommand (Just "") = Right SkipCmd
parseCommand (Just x) = parse commands "" x

evalCommand :: Manager -> (Manager -> InputT IO ()) -> InputCommand -> InputT IO ()
evalCommand _ _ ExitCmd = return ()
evalCommand mngr loop SkipCmd = loop mngr
evalCommand mngr loop (EchoCmd str) = outputStrLn str >> loop mngr
evalCommand mngr loop (CowSayCmd str) = liftIO (system ("cowsay " ++ str)) >> loop mngr
evalCommand mngr loop MyIpCmd =
  liftIO (runClientM getIP (mkClientEnv mngr myURL))
    >>= outputStrLn . show >> loop mngr
evalCommand mngr loop HiCmd = outputStrLn "hi!" >> loop mngr

run :: IO ()
run = runInputT defaultSettings welcome
  where
    welcome :: InputT IO ()
    welcome = do
      mngr <- liftIO $ newManager defaultManagerSettings
      outputStrLn "Welcome to DFS client! Type 'help' for help;)"
      loop mngr
      outputStrLn "Goodbye!"
    loop :: Manager -> InputT IO ()
    loop mngr = do
      minput <- getInputLine ">>= "
      case parseCommand minput of
        (Left msg) -> outputStrLn ("Wrong command, " ++ show msg) >> loop mngr
        (Right cmd) -> evalCommand mngr loop cmd
