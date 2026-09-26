module Presentation.Server.API (
    APIServerDependencies (..),
    articleAPIServer,
) where

import Cloudflare.Workers.Entrypoint.Fetch (FetchHandler)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Presentation.API (ArticleAdminAPI, ArticleAdminRoutes (..))
import Presentation.Handler.API.Proofread (
    ProofreadHandlerDependencies,
    proofreadHandler,
 )
import Presentation.Handler.API.RequestExcerptRegeneration (
    RegenerationHandlerDependencies,
    requestExcerptRegenerationHandler,
 )
import Servant.Cloudflare.Workers.Generic (AsWorker)
import Servant.Cloudflare.Workers.Server (Context (EmptyContext), serveWithContext)

data APIServerDependencies = APIServerDependencies
    { proofread :: ProofreadHandlerDependencies
    , requestExcerptRegeneration :: RegenerationHandlerDependencies
    }

articleAPIServer :: FetchHandler APIServerDependencies
articleAPIServer request dependencies context =
    serveWithContext
        (Proxy @ArticleAdminAPI)
        EmptyContext
        (articleRoutes dependencies)
        request
        context
        ()

articleRoutes ::
    APIServerDependencies -> Maybe Text -> Maybe Text -> ArticleAdminRoutes (AsWorker ())
articleRoutes dependencies actor correlation =
    ArticleAdminRoutes
        { proofread = proofreadHandler dependencies.proofread actor correlation
        , requestExcerptRegeneration =
            requestExcerptRegenerationHandler
                dependencies.requestExcerptRegeneration
                actor
                correlation
        }
