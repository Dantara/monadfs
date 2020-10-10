{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Client.Types where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader
import Control.Monad.State
import Network.HTTP.Client (Manager)
import Servant.Client

data InputCommand
  = ExitCmd
  | SkipCmd
  | EchoCmd String
  | CowSayCmd String
  | MyIpCmd
  | HelpCmd
  | InitCmd
  | PWDCmd
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

data Environment = Environment
  { envManager :: Manager,
    envBaseUrl :: BaseUrl
  }

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
