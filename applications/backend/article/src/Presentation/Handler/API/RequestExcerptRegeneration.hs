module Presentation.Handler.API.RequestExcerptRegeneration (
    RegenerationHandlerDependencies (..),
    requestExcerptRegenerationHandler,
) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import "article" Domain.Article.Common (articleIdentifierText, newArticleIdentifier)
import Presentation.API (CorrelatedResponse, RegenerationResponse (..))
import Presentation.Handler.API.Error (domainErrorResponse, publicError)
import Presentation.Handler.API.Metadata (MetadataDependencies, newCommand)
import Servant.API (addHeader)
import Servant.Cloudflare.Workers.Handler (Handler)
import Shared.Domain.Error (DomainError)
import Shared.UseCase.Command (Command (..))
import "article" UseCase.RequestExcerptRegeneration (RequestExcerptRegenerationPayload (..))

data RegenerationHandlerDependencies = RegenerationHandlerDependencies
    { metadata :: MetadataDependencies
    , execute ::
        Command RequestExcerptRegenerationPayload -> IO (Either DomainError Text)
    }

requestExcerptRegenerationHandler ::
    RegenerationHandlerDependencies ->
    Maybe Text ->
    Maybe Text ->
    Text ->
    Handler env (CorrelatedResponse RegenerationResponse)
requestExcerptRegenerationHandler dependencies actorHeader correlationHeader rawArticle = do
    (metadata, correlation) <- newCommand dependencies.metadata actorHeader correlationHeader ()
    article <- either
        (const (throwError (publicError 400 "invalid_article_identifier" correlation)))
        pure
        (newArticleIdentifier rawArticle)
    let command = metadata{payload = RequestExcerptRegenerationPayload article}
    result <- liftIO (dependencies.execute command)
    identifier <- either (throwError . domainErrorResponse correlation) pure result
    pure (addHeader correlation (RegenerationResponse (articleIdentifierText article) identifier))
