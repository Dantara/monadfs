module NameServer.Internals where


import           MonadFS.API.Types
import           Servant.Client

addrToBaseUrl :: ServerAddr -> BaseUrl
addrToBaseUrl (ServerAddr url port') = BaseUrl Http url port' ""
