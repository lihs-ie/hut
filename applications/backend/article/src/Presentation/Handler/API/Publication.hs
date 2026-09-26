{-# LANGUAGE PackageImports #-}

module Presentation.Handler.API.Publication (
    PublicationHandlerDependencies (..),
    publishHandler,
    takeDownHandler,
    resumePublicationHandler,
    discardArticleHandler,
) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import "article" Domain.Article (Article (..), ArticleIdentifier, articleIdentifierText)
import "article" Domain.Article.Draft (ReadyToPublish)
import "article" Domain.Article.Private (PrivateArticle)
import "article" Domain.Article.Published (PublishedArticle)
import Presentation.API (CorrelatedResponse, DiscardResponse (..))
import Presentation.API.ArticleView (ArticleView, articleView)
import Presentation.Handler.API.Error (domainErrorResponse)
import Presentation.Handler.API.Metadata (
    MetadataDependencies,
    newCommand,
    parseArticleIdentifier,
 )
import Servant.API (addHeader)
import Servant.Cloudflare.Workers.Handler (Handler)
import "article" UseCase.DiscardArticle (DiscardArticleCommand, DiscardArticlePayload (..))
import "article" UseCase.Publish (PublishCommand, PublishPayload (..))
import "article" UseCase.ResumePublication (ResumePublicationCommand, ResumePublicationPayload (..))
import "article" UseCase.TakeDown (TakeDownCommand, TakeDownPayload (..))
import "shared" Shared.Domain.Error (DomainError)
import "shared" Shared.UseCase.Command (Command (..))

data PublicationHandlerDependencies = PublicationHandlerDependencies
    { metadata :: MetadataDependencies
    , publish :: PublishCommand -> IO (Either DomainError PublishedArticle)
    , takeDown :: TakeDownCommand -> IO (Either DomainError PrivateArticle)
    , resumePublication :: ResumePublicationCommand -> IO (Either DomainError ReadyToPublish)
    , discardArticle :: DiscardArticleCommand -> IO (Either DomainError ArticleIdentifier)
    }

publishHandler ::
    PublicationHandlerDependencies -> Maybe Text -> Maybe Text -> Text ->
    Handler env (CorrelatedResponse ArticleView)
publishHandler dependencies actorHeader correlationHeader rawArticle =
    handleArticle dependencies.metadata actorHeader correlationHeader rawArticle
        PublishPayload dependencies.publish Published

takeDownHandler ::
    PublicationHandlerDependencies -> Maybe Text -> Maybe Text -> Text ->
    Handler env (CorrelatedResponse ArticleView)
takeDownHandler dependencies actorHeader correlationHeader rawArticle =
    handleArticle dependencies.metadata actorHeader correlationHeader rawArticle
        TakeDownPayload dependencies.takeDown Private

resumePublicationHandler ::
    PublicationHandlerDependencies -> Maybe Text -> Maybe Text -> Text ->
    Handler env (CorrelatedResponse ArticleView)
resumePublicationHandler dependencies actorHeader correlationHeader rawArticle =
    handleArticle dependencies.metadata actorHeader correlationHeader rawArticle
        ResumePublicationPayload dependencies.resumePublication Ready

discardArticleHandler ::
    PublicationHandlerDependencies -> Maybe Text -> Maybe Text -> Text ->
    Handler env (CorrelatedResponse DiscardResponse)
discardArticleHandler dependencies actorHeader correlationHeader rawArticle = do
    (metadata, correlation) <- newCommand dependencies.metadata actorHeader correlationHeader ()
    identifier <- parseArticleIdentifier correlation rawArticle
    let command = metadata{payload = DiscardArticlePayload identifier}
    result <- liftIO (dependencies.discardArticle command)
    discarded <- either (throwError . domainErrorResponse correlation) pure result
    pure (addHeader correlation (DiscardResponse (articleIdentifierText discarded)))

handleArticle ::
    MetadataDependencies -> Maybe Text -> Maybe Text -> Text ->
    (ArticleIdentifier -> payload) ->
    (Command payload -> IO (Either DomainError result)) ->
    (result -> Article) ->
    Handler env (CorrelatedResponse ArticleView)
handleArticle metadata actorHeader correlationHeader rawArticle newPayload execute wrap = do
    (command, correlation) <- newCommand metadata actorHeader correlationHeader ()
    identifier <- parseArticleIdentifier correlation rawArticle
    result <- liftIO (execute command{payload = newPayload identifier})
    article <- either (throwError . domainErrorResponse correlation) pure result
    pure (addHeader correlation (articleView (wrap article)))
