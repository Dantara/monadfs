{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Client.Types where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader
import Control.Monad.State
import Data.Proxy (Proxy (..))
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Servant.Client
import System.Console.Haskeline
import System.Process (system)
import Text.Parsec

data MyEnv = MyEnv Manager BaseUrl (InputT IO ())

data InputCommand
  = ExitCmd
  | SkipCmd
  | HelpCmd
  | InitCmd
  | PWDCmd
  | MyIpCmd
  | EchoCmd String
  | CowSayCmd String
  | TouchCmd String
  | GetCmd String
  | PutCmd String
  | RemoveCmd String
  | FileInfoCmd String
  | CopyCmd String String
  | MoveCmd String String
  | MakeDirCmd String
  | RemoveDirCmd String
  | DirInfoCmd String
  | ChangeDirCmd String
  deriving (Show, Eq)

newtype Environment = Environment Manager

newtype CLIClient r s a = CLIClient {unClient :: (ReaderT r (StateT s IO) a)}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadReader r)
  deriving newtype (MonadState s)
  deriving newtype (MonadIO, MonadThrow, MonadCatch, MonadMask)

runCLIClient :: r -> s -> CLIClient r s a -> IO a
runCLIClient env st =
  flip evalStateT st
    . flip runReaderT env
    . unClient
