module UseCase.Publish (
    PublishPayload (..),
    PublishCommand,
    PublishResult (..),
    Dependencies (..),
    publish,
) where

import Domain.Article
import Domain.Article.Published (PublishedArticle)
import Domain.Article.Published qualified as Domain
import Shared.Domain.Common.Transaction (Transaction, TransactionManager, abort, fromEither, runTransaction)
import Shared.Domain.Error (DomainError, createOperationNotAllowed)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.UseCase.Command (Command (..), commandContext)
import Shared.UseCase.Outbox (Append)
import UseCase.Helper
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

newtype PublishPayload = PublishPayload {article :: ArticleIdentifier}
    deriving stock (Show, Eq)

type PublishCommand = Command PublishPayload

data PublishResult = PublishResult
    { article :: PublishedArticle
    , events :: Events (ArticleEventsFor 'Result.Publish)
    }

data Dependencies context m = Dependencies
    { transactionManager :: TransactionManager context m
    , findArticle :: FindArticle (Transaction context m)
    , persistArticle :: PersistArticle (Transaction context m)
    , appendEvents :: Append (ArticleEventsFor 'Result.Publish) (Transaction context m)
    }

publish :: (Monad m) => Dependencies context m -> PublishCommand -> m (Either DomainError PublishResult)
publish dependencies command =
    runTransaction dependencies.transactionManager $ do
        source <- requireArticle dependencies.findArticle command.payload.article
        article <- case source of
            Ready draft -> fromEither (Domain.publish command.timestamp draft)
            _ -> abort (createOperationNotAllowed "Publish" "only ready drafts can be published")
        let events = Events [Here (DomainEvent article.identifier)]
        dependencies.persistArticle (Published article)
        dependencies.appendEvents (commandContext command) events
        pure (PublishResult article events)
