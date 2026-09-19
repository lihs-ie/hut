module UseCase.DiscardArticle (
    DiscardArticlePayload (..),
    DiscardArticleCommand,
    DiscardArticleResult (..),
    Dependencies (..),
    discardArticle,
) where

import Domain.Article (Article (..), articleIdentifier)
import Domain.Article.Common (ArticleIdentifier, articleIdentifierText)
import Shared.Domain.Error (DomainError, createAggregateNotFound, createOperationNotAllowed, createUnexpectedError)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.UseCase.Command (Command (..))
import UseCase.Persistence (LoadForDiscard, LoadedForDiscard (..), commandContext)
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

newtype DiscardArticlePayload = DiscardArticlePayload {article :: ArticleIdentifier}
    deriving stock (Show, Eq)

type DiscardArticleCommand = Command DiscardArticlePayload

data DiscardArticleResult = DiscardArticleResult
    { article :: ArticleIdentifier
    , events :: Events (ArticleEventsFor 'Result.DiscardArticle)
    }

newtype Dependencies m = Dependencies {loadArticle :: LoadForDiscard m}

discardArticle ::
    (Monad m) =>
    Dependencies m ->
    DiscardArticleCommand ->
    m (Either DomainError DiscardArticleResult)
discardArticle dependencies command = do
    loaded <- dependencies.loadArticle command.payload.article
    case loaded of
        Left err -> pure (Left err)
        Right Nothing ->
            pure (Left (createAggregateNotFound "Article" (articleIdentifierText command.payload.article)))
        Right (Just snapshot)
            | articleIdentifier snapshot.article /= command.payload.article ->
                pure (Left (createUnexpectedError "Article" "loaded identity does not match request"))
            | otherwise -> case snapshot.article of
                Published _ ->
                    pure (Left (createOperationNotAllowed "DiscardArticle" "published articles must be taken down first"))
                Unvalidated _ -> commit snapshot
                Proofreaded _ -> commit snapshot
                Ready _ -> commit snapshot
                Private _ -> commit snapshot
  where
    commit snapshot = do
        let article = articleIdentifier snapshot.article
            events = Events [Here (DomainEvent article)]
        saved <- snapshot.commitDiscard (commandContext command) events
        pure (DiscardArticleResult article events <$ saved)
