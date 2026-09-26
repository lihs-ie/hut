module Media.Infrastructure.Cache.CloudflarePurger (
    newCloudflareCachePurger,
) where

import Cloudflare.Workers.Binding.Secret (Secret, revealSecret)
import Cloudflare.Workers.Binding.Var (Var, unVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Client.Core (BaseUrl (BaseUrl), Scheme (Https), clientIn)
import Servant.Cloudflare.Workers.Client.Fetch (FetchClient, runFetchClient)
import Shared.Domain.Error (DomainError, createServiceUnavailable)

type CloudflarePurgeAPI =
    "client"
        :> "v4"
        :> "zones"
        :> Capture "zoneIdentifier" Text
        :> "purge_cache"
        :> Header' '[Required, Strict] "Authorization" Text
        :> ReqBody '[JSON] PurgeRequest
        :> Post '[JSON] PurgeResponse

newtype PurgeRequest = PurgeRequest
    { files :: [Text]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

newtype PurgeResponse = PurgeResponse
    { success :: Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

newCloudflareCachePurger :: Var -> Secret -> Text -> IO ()
newCloudflareCachePurger zone token url = do
    outcome <-
        try @SomeException
            ( runFetchClient
                (purgeClient (unVar zone) authorization (PurgeRequest [url]))
                cloudflareAPI
            )
    response <- either (const (throwIO cachePurgeUnavailable)) pure outcome
    unless response.success (throwIO cachePurgeUnavailable)
  where
    authorization = "Bearer " <> revealSecret token

cachePurgeUnavailable :: DomainError
cachePurgeUnavailable =
    createServiceUnavailable
        "CloudflareCache"
        "purge cached image failed"

purgeClient :: Text -> Text -> PurgeRequest -> FetchClient PurgeResponse
purgeClient = clientIn (Proxy @CloudflarePurgeAPI) (Proxy @FetchClient)

cloudflareAPI :: BaseUrl
cloudflareAPI = BaseUrl Https "api.cloudflare.com" 443 ""
