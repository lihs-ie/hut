{-# LANGUAGE PackageImports #-}

module Presentation.Handler.API.DraftWriting (
    DraftWritingDependencies (..),
    jotDownHandler,
    amendDraftHandler,
    reviseExcerptHandler,
) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import "article" Domain.Article (Article (..), DraftInput (..))
import "article" Domain.Article.Draft (ReadyToPublish, UnvalidatedDraft)
import Presentation.API (CorrelatedResponse, DraftRequest (..), ExcerptRequest (..))
import Presentation.API.ArticleView (ArticleView, articleView)
import Presentation.Handler.API.Error (domainErrorResponse)
import Presentation.Handler.API.Metadata (
    MetadataDependencies,
    newCommand,
    parseArticleIdentifier,
 )
import Servant.API (addHeader)
import Servant.Cloudflare.Workers.Handler (Handler)
import "article" UseCase.AmendDraft (AmendDraftPayload (..), AmendDraftCommand)
import "article" UseCase.JotDown (JotDownCommand)
import "article" UseCase.PrepareToPublish (PrepareToPublishCommand, PrepareToPublishPayload (..))
import "shared" Shared.Domain.Error (DomainError)
import "shared" Shared.UseCase.Command (Command (..))

data DraftWritingDependencies = DraftWritingDependencies
    { metadata :: MetadataDependencies
    , jotDown :: JotDownCommand -> IO (Either DomainError UnvalidatedDraft)
    , amendDraft :: AmendDraftCommand -> IO (Either DomainError UnvalidatedDraft)
    , reviseExcerpt :: PrepareToPublishCommand -> IO (Either DomainError ReadyToPublish)
    }

jotDownHandler ::
    DraftWritingDependencies -> Maybe Text -> Maybe Text -> DraftRequest ->
    Handler env (CorrelatedResponse ArticleView)
jotDownHandler dependencies actorHeader correlationHeader input = do
    (metadata, correlation) <- newCommand dependencies.metadata actorHeader correlationHeader ()
    let command = metadata{payload = draftInput input}
    result <- liftIO (dependencies.jotDown command)
    draft <- either (throwError . domainErrorResponse correlation) pure result
    pure (addHeader correlation (articleView (Unvalidated draft)))

amendDraftHandler ::
    DraftWritingDependencies -> Maybe Text -> Maybe Text -> Text -> DraftRequest ->
    Handler env (CorrelatedResponse ArticleView)
amendDraftHandler dependencies actorHeader correlationHeader rawArticle input = do
    (metadata, correlation) <- newCommand dependencies.metadata actorHeader correlationHeader ()
    identifier <- parseArticleIdentifier correlation rawArticle
    let command = metadata{payload = AmendDraftPayload identifier input.title input.body input.slug input.tags}
    result <- liftIO (dependencies.amendDraft command)
    draft <- either (throwError . domainErrorResponse correlation) pure result
    pure (addHeader correlation (articleView (Unvalidated draft)))

reviseExcerptHandler ::
    DraftWritingDependencies -> Maybe Text -> Maybe Text -> Text -> ExcerptRequest ->
    Handler env (CorrelatedResponse ArticleView)
reviseExcerptHandler dependencies actorHeader correlationHeader rawArticle input = do
    (metadata, correlation) <- newCommand dependencies.metadata actorHeader correlationHeader ()
    identifier <- parseArticleIdentifier correlation rawArticle
    let command = metadata{payload = ReviseExcerpt identifier input.excerpt}
    result <- liftIO (dependencies.reviseExcerpt command)
    ready <- either (throwError . domainErrorResponse correlation) pure result
    pure (addHeader correlation (articleView (Ready ready)))

draftInput :: DraftRequest -> DraftInput
draftInput input = DraftInput input.title input.body input.slug input.tags
