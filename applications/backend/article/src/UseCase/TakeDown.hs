module UseCase.TakeDown (
    TakeDownPayload (..),
    TakeDownCommand,
    TakeDownResult (..),
    Dependencies (..),
    takeDown,
) where

import Domain.Article (Article (..), articleIdentifier)
import Domain.Article.Common (ArticleIdentifier, articleIdentifierText)
import Domain.Article.Private (PrivateArticle)
import Domain.Article.Private qualified as Domain
import Shared.Domain.Error (
    DomainError,
    createAggregateNotFound,
    createOperationNotAllowed,
    createUnexpectedError,
 )
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.UseCase.Command (Command (..))
import UseCase.Persistence (LoadForTakeDown, LoadedForTakeDown (..), commandContext)
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

newtype TakeDownPayload = TakeDownPayload {article :: ArticleIdentifier}
    deriving stock (Show, Eq)

type TakeDownCommand = Command TakeDownPayload

data TakeDownResult = TakeDownResult
    { article :: PrivateArticle
    , events :: Events (ArticleEventsFor 'Result.TakeDown)
    }

newtype Dependencies m = Dependencies
    { loadArticle :: LoadForTakeDown m
    }

takeDown ::
    (Monad m) =>
    Dependencies m ->
    TakeDownCommand ->
    m (Either DomainError TakeDownResult)
takeDown dependencies command = do
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
                Published source ->
                    case Domain.takeDown command.timestamp source of
                        Left err -> pure (Left err)
                        Right article -> do
                            let events = Events [Here (DomainEvent article.identifier)]
                            saved <- snapshot.saveTakeDown (commandContext command) article events
                            pure (TakeDownResult article events <$ saved)
                _ ->
                    pure (Left (createOperationNotAllowed "TakeDown" "only published articles can be taken down"))
