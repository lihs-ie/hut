module UseCase.DiscardArticle (
    DiscardArticlePayload (..),
    DiscardArticleCommand,
    DiscardArticleResult (..),
    Dependencies (..),
    discardArticle,
) where

import Domain.Article
import Shared.Domain.Common.Transaction (Transaction, TransactionManager, abort, runTransaction)
import Shared.Domain.Error (DomainError, createOperationNotAllowed)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.UseCase.Command (Command (..), commandContext)
import Shared.UseCase.Outbox (Append)
import UseCase.Helper
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

newtype DiscardArticlePayload = DiscardArticlePayload {article :: ArticleIdentifier}
    deriving stock (Show, Eq)

type DiscardArticleCommand = Command DiscardArticlePayload

data DiscardArticleResult = DiscardArticleResult
    { article :: ArticleIdentifier
    , events :: Events (ArticleEventsFor 'Result.DiscardArticle)
    }

data Dependencies context m = Dependencies
    { transactionManager :: TransactionManager context m
    , findArticle :: FindArticle (Transaction context m)
    , terminateArticle :: TerminateArticle (Transaction context m)
    , appendEvents :: Append (ArticleEventsFor 'Result.DiscardArticle) (Transaction context m)
    }

discardArticle ::
    (Monad m) =>
    Dependencies context m ->
    DiscardArticleCommand ->
    m (Either DomainError DiscardArticleResult)
discardArticle dependencies command = runTransaction dependencies.transactionManager $ do
    source <- requireArticle dependencies.findArticle command.payload.article
    case source of
        Published _ ->
            abort
                (createOperationNotAllowed "DiscardArticle" "published articles must be taken down first")
        _ -> pure ()
    let article = articleIdentifier source
        events = Events [Here (DomainEvent article)]
    dependencies.terminateArticle article
    dependencies.appendEvents (commandContext command) events
    pure (DiscardArticleResult article events)
