module UseCase.JotDown (
    JotDownPayload,
    JotDownCommand,
    JotDownResult (..),
    Dependencies (..),
    jotDown,
) where

import Domain.Article
import Domain.Article.Draft (UnvalidatedDraft, newUnvalidatedDraft)
import Domain.Article.Event (draftImageReferences)
import Shared.Domain.Common.Transaction (Transaction, TransactionManager, fromEither, runTransaction)
import Shared.Domain.Error (DomainError)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.UseCase.Command (Command (..), commandContext)
import Shared.UseCase.Outbox (Append)
import UseCase.Result (ArticleEventsFor, ArticleUseCase (JotDown))

type JotDownPayload = DraftInput
type JotDownCommand = Command JotDownPayload

data JotDownResult = JotDownResult
    { article :: UnvalidatedDraft
    , events :: Events (ArticleEventsFor 'JotDown)
    }

data Dependencies context m = Dependencies
    { transactionManager :: TransactionManager context m
    , newArticleIdentifier :: m (Either DomainError ArticleIdentifier)
    , extractImageReferences :: ExtractImageReferences
    , persistArticle :: PersistArticle (Transaction context m)
    , appendEvents :: Append (ArticleEventsFor 'JotDown) (Transaction context m)
    }

jotDown :: (Monad m) => Dependencies context m -> JotDownCommand -> m (Either DomainError JotDownResult)
jotDown dependencies command =
    case newDraftContent dependencies.extractImageReferences command.payload of
        Left err -> pure (Left err)
        Right content -> do
            generated <- dependencies.newArticleIdentifier
            case generated of
                Left err -> pure (Left err)
                Right identifier -> runTransaction dependencies.transactionManager $ do
                    article <- fromEither (newUnvalidatedDraft identifier command.timestamp content)
                    let events = Events [Here (DomainEvent (draftImageReferences article))]
                    dependencies.persistArticle (Unvalidated article)
                    dependencies.appendEvents (commandContext command) events
                    pure (JotDownResult article events)
