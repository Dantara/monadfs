module NameServer.Server.Controllers where

import           MonadFS.API.NameServer
import           NameServer.Server.Controllers.Dir
import           NameServer.Server.Controllers.File
import           NameServer.Server.Controllers.Root
import           NameServer.Types
import           Servant

controllers :: ServerT NameServerAPI AppM
controllers = initController
  :<|> (fileCreateController :<|> fileReadController
       :<|> fileWriteController :<|> fileDeleteController
       :<|> fileInfoController :<|> fileCopyController
       :<|> fileMoveController)
  :<|> (dirCreateController :<|> dirDeleteController
       :<|> dirInfoController :<|> dirExistsController)
