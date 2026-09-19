module UseCase.Publish (
    PublishPayload (..),
    PublishCommand,
    PublishResult (..),
    Dependencies (..),
    publish,
) where

import Domain.Article (Article (..), articleIdentifier)
import Domain.Article.Common (ArticleIdentifier, articleIdentifierText)
import Domain.Article.Published (PublishedArticle)
import Domain.Article.Published qualified as Domain
import Shared.Domain.Error (
    DomainError,
    createAggregateNotFound,
    createOperationNotAllowed,
    createUnexpectedError,
 )
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.UseCase.Command (Command (..))
import UseCase.Persistence (LoadForPublication, LoadedForPublication (..), commandContext)
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

newtype PublishPayload = PublishPayload {article :: ArticleIdentifier}
    deriving stock (Show, Eq)

type PublishCommand = Command PublishPayload

data PublishResult = PublishResult
    { article :: PublishedArticle
    , events :: Events (ArticleEventsFor 'Result.Publish)
    }

newtype Dependencies m = Dependencies
    { loadArticle :: LoadForPublication m
    }

publish ::
    (Monad m) =>
    Dependencies m ->
    PublishCommand ->
    m (Either DomainError PublishResult)
publish dependencies command = do
    loaded <- dependencies.loadArticle command.payload.article
    case loaded of
        Left err -> pure (Left err)
        Right Nothing ->
            pure
                (Left (createAggregateNotFound "Article" (articleIdentifierText command.payload.article)))
        Right (Just snapshot)
            | articleIdentifier snapshot.article /= command.payload.article ->
                pure (Left (createUnexpectedError "Article" "loaded identity does not match request"))
            | otherwise -> case snapshot.article of
                Ready source ->
                    case Domain.publish command.timestamp source of
                        Left err -> pure (Left err)
                        Right article -> do
                            let events = Events [Here (DomainEvent article.identifier)]
                            saved <- snapshot.savePublication (commandContext command) article events
                            pure (PublishResult article events <$ saved)
                _ ->
                    pure (Left (createOperationNotAllowed "Publish" "only ready drafts can be published"))
