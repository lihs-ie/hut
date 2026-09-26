module UseCase.ResumePublication (
    ResumePublicationPayload (..),
    ResumePublicationCommand,
    ResumePublicationResult (..),
    Dependencies (..),
    resumePublication,
) where

import Domain.Article
import Domain.Article.Draft (ReadyToPublish)
import Domain.Article.Private qualified as Private
import Shared.Domain.Common.Transaction (Transaction, TransactionManager, abort, fromEither, runTransaction)
import Shared.Domain.Error (DomainError, createOperationNotAllowed)
import Shared.Domain.Event (Events (..))
import Shared.UseCase.Command (Command (..))
import UseCase.Helper
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

newtype ResumePublicationPayload = ResumePublicationPayload {article :: ArticleIdentifier}
    deriving stock (Show, Eq)

type ResumePublicationCommand = Command ResumePublicationPayload

data ResumePublicationResult = ResumePublicationResult
    { article :: ReadyToPublish
    , events :: Events (ArticleEventsFor 'Result.ResumePublication)
    }

data Dependencies context m = Dependencies
    { transactionManager :: TransactionManager context m
    , findArticle :: FindArticle (Transaction context m)
    , persistArticle :: PersistArticle (Transaction context m)
    }

resumePublication ::
    (Monad m) =>
    Dependencies context m ->
    ResumePublicationCommand ->
    m (Either DomainError ResumePublicationResult)
resumePublication dependencies command =
    runTransaction dependencies.transactionManager $ do
        source <- requireArticle dependencies.findArticle command.payload.article
        article <- case source of
            Private draft -> fromEither (Private.resumePublication command.timestamp draft)
            _ ->
                abort
                    (createOperationNotAllowed "ResumePublication" "only private articles can resume publication")
        let events = Events []
        dependencies.persistArticle (Ready article)
        pure (ResumePublicationResult article events)
