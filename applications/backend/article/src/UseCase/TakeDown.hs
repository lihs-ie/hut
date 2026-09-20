module UseCase.TakeDown (
    TakeDownPayload (..),
    TakeDownCommand,
    TakeDownResult (..),
    Dependencies (..),
    takeDown,
) where

import Domain.Article
import Domain.Article.Private (PrivateArticle)
import Domain.Article.Private qualified as Domain
import Shared.Domain.Common.Transaction (Transaction, TransactionManager, abort, fromEither, runTransaction)
import Shared.Domain.Error (DomainError, createOperationNotAllowed)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.UseCase.Command (Command (..), commandContext)
import Shared.UseCase.Outbox (Append)
import UseCase.Helper
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

newtype TakeDownPayload = TakeDownPayload {article :: ArticleIdentifier}
    deriving stock (Show, Eq)

type TakeDownCommand = Command TakeDownPayload

data TakeDownResult = TakeDownResult
    { article :: PrivateArticle
    , events :: Events (ArticleEventsFor 'Result.TakeDown)
    }

data Dependencies context m = Dependencies
    { transactionManager :: TransactionManager context m
    , findArticle :: FindArticle (Transaction context m)
    , persistArticle :: PersistArticle (Transaction context m)
    , appendEvents :: Append (ArticleEventsFor 'Result.TakeDown) (Transaction context m)
    }

takeDown :: (Monad m) => Dependencies context m -> TakeDownCommand -> m (Either DomainError TakeDownResult)
takeDown dependencies command =
    runTransaction dependencies.transactionManager $ do
        source <- requireArticle dependencies.findArticle command.payload.article
        article <- case source of
            Published draft -> fromEither (Domain.takeDown command.timestamp draft)
            _ -> abort (createOperationNotAllowed "TakeDown" "only published articles can be taken down")
        let events = Events [Here (DomainEvent article.identifier)]
        dependencies.persistArticle (Private article)
        dependencies.appendEvents (commandContext command) events
        pure (TakeDownResult article events)
