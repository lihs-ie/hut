module Presentation.Handler.API.Proofread (
    ProofreadHandlerDependencies (..),
    proofreadHandler,
) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import "article" Domain.Article.Common (articleIdentifierText, newArticleIdentifier)
import Presentation.API (CorrelatedResponse, ProofreadResponse (..))
import Presentation.Handler.API.Error (domainErrorResponse, publicError)
import Presentation.Handler.API.Metadata (MetadataDependencies, newCommand)
import Servant.API (addHeader)
import Servant.Cloudflare.Workers.Handler (Handler)
import Shared.Domain.Error (DomainError)
import Shared.UseCase.Command (Command (..))
import "article" UseCase.Proofread (ProofreadPayload (..))

data ProofreadHandlerDependencies = ProofreadHandlerDependencies
    { metadata :: MetadataDependencies
    , execute :: Command ProofreadPayload -> IO (Either DomainError ())
    }

proofreadHandler ::
    ProofreadHandlerDependencies ->
    Maybe Text ->
    Maybe Text ->
    Text ->
    Handler env (CorrelatedResponse ProofreadResponse)
proofreadHandler dependencies actorHeader correlationHeader rawArticle = do
    (metadata, correlation) <- newCommand dependencies.metadata actorHeader correlationHeader ()
    article <- either
        (const (throwError (publicError 400 "invalid_article_identifier" correlation)))
        pure
        (newArticleIdentifier rawArticle)
    let command = metadata{payload = ProofreadPayload article}
    completed <- liftIO (dependencies.execute command)
    either (throwError . domainErrorResponse correlation) pure completed
    pure (addHeader correlation (ProofreadResponse (articleIdentifierText article) "proofreaded"))
