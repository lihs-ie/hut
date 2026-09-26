module Presentation.Server.API (
    APIServerDependencies (..),
    articleAPIServer,
) where

import Cloudflare.Workers.Entrypoint.Fetch (FetchHandler)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Presentation.API (ArticleAPI, ArticleAdminRoutes (..), ArticleReaderRoutes (..))
import Presentation.Handler.API.Reading (
    ReadingHandlerDependencies,
    browseAdminHandler,
    browseReaderHandler,
    checkSlugHandler,
    readArticleHandler,
    viewAdminHandler,
 )
import Presentation.Handler.API.DraftWriting (
    DraftWritingDependencies,
    amendDraftHandler,
    jotDownHandler,
    reviseExcerptHandler,
 )
import Presentation.Handler.API.Publication (
    PublicationHandlerDependencies,
    discardArticleHandler,
    publishHandler,
    resumePublicationHandler,
    takeDownHandler,
 )
import Presentation.Handler.API.Proofread (
    ProofreadHandlerDependencies,
    proofreadHandler,
 )
import Presentation.Handler.API.RequestExcerptRegeneration (
    RegenerationHandlerDependencies,
    requestExcerptRegenerationHandler,
 )
import Servant.API ((:<|>) (..))
import Servant.Cloudflare.Workers.Generic (AsWorker)
import Servant.Cloudflare.Workers.Server (Context (EmptyContext), serveWithContext)

data APIServerDependencies = APIServerDependencies
    { proofread :: ProofreadHandlerDependencies
    , requestExcerptRegeneration :: RegenerationHandlerDependencies
    , reading :: ReadingHandlerDependencies
    , draftWriting :: DraftWritingDependencies
    , publication :: PublicationHandlerDependencies
    }

articleAPIServer :: FetchHandler APIServerDependencies
articleAPIServer request dependencies context =
    serveWithContext
        (Proxy @ArticleAPI)
        EmptyContext
        (articleRoutes dependencies :<|> readerRoutes dependencies)
        request
        context
        ()

articleRoutes ::
    APIServerDependencies -> Maybe Text -> Maybe Text -> ArticleAdminRoutes (AsWorker ())
articleRoutes dependencies actor correlation =
    ArticleAdminRoutes
        { jotDown = jotDownHandler dependencies.draftWriting actor correlation
        , browseArticles = browseAdminHandler dependencies.reading actor correlation
        , viewArticle = viewAdminHandler dependencies.reading actor correlation
        , amendDraft = amendDraftHandler dependencies.draftWriting actor correlation
        , reviseExcerpt = reviseExcerptHandler dependencies.draftWriting actor correlation
        , publish = publishHandler dependencies.publication actor correlation
        , takeDown = takeDownHandler dependencies.publication actor correlation
        , resumePublication = resumePublicationHandler dependencies.publication actor correlation
        , discardArticle = discardArticleHandler dependencies.publication actor correlation
        , checkSlugAvailability = checkSlugHandler dependencies.reading actor correlation
        , proofread = proofreadHandler dependencies.proofread actor correlation
        , requestExcerptRegeneration =
            requestExcerptRegenerationHandler
                dependencies.requestExcerptRegeneration
                actor
                correlation
        }

readerRoutes :: APIServerDependencies -> Maybe Text -> ArticleReaderRoutes (AsWorker ())
readerRoutes dependencies correlation =
    ArticleReaderRoutes
        { browseArticles = browseReaderHandler dependencies.reading correlation
        , readArticle = readArticleHandler dependencies.reading correlation
        }
