module UseCase.RequestExcerptRegeneration (
    RequestExcerptRegenerationPayload (..),
    RequestExcerptRegenerationCommand,
    RequestExcerptRegenerationResult (..),
    RecordRegenerationRequest,
    Dependencies (..),
    requestExcerptRegeneration,
) where

import Domain.Article (Article (..), ArticleIdentifier, FindArticle)
import Domain.Article.Draft qualified as Draft
import Shared.Domain.Common.Transaction (Transaction, TransactionManager, abort, runTransaction)
import Shared.Domain.Error (DomainError, createOperationNotAllowed)
import Shared.Domain.Event (Events (..))
import Shared.UseCase.Command (Command (..), commandContext)
import UseCase.Helper (requireArticle)
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

newtype RequestExcerptRegenerationPayload = RequestExcerptRegenerationPayload
    { article :: ArticleIdentifier
    }
    deriving stock (Show, Eq)

type RequestExcerptRegenerationCommand = Command RequestExcerptRegenerationPayload

data RequestExcerptRegenerationResult requestIdentifier = RequestExcerptRegenerationResult
    { article :: Draft.ProofreadedDraft
    , requestIdentifier :: requestIdentifier
    , events :: Events (ArticleEventsFor 'Result.RequestExcerptRegeneration)
    }

-- The adapter obtains the revision observed by FindArticle from this transaction.
-- For an active (article, revision) request it returns the existing request;
-- otherwise it atomically records a new request and its Outbox instruction.
-- A terminal DLQ request is not active. The identifier and revision are
-- adapter-owned; the completion consumer must reject stale identifiers.
type RecordRegenerationRequest requestIdentifier m =
    Command () -> ArticleIdentifier -> m requestIdentifier

data Dependencies context m requestIdentifier = Dependencies
    { transactionManager :: TransactionManager context m
    , findArticle :: FindArticle (Transaction context m)
    , recordRegenerationRequest ::
        RecordRegenerationRequest requestIdentifier (Transaction context m)
    }

requestExcerptRegeneration ::
    (Monad m) =>
    Dependencies context m requestIdentifier ->
    RequestExcerptRegenerationCommand ->
    m (Either DomainError (RequestExcerptRegenerationResult requestIdentifier))
requestExcerptRegeneration dependencies command =
    runTransaction dependencies.transactionManager $ do
        found <- requireArticle dependencies.findArticle command.payload.article
        case found of
            Proofreaded draft -> do
                requestIdentifier <-
                    dependencies.recordRegenerationRequest
                        (commandContext command)
                        (Draft.draftIdentifier draft)
                pure
                    ( RequestExcerptRegenerationResult
                        draft
                        requestIdentifier
                        (Events [])
                    )
            _ ->
                abort
                    ( createOperationNotAllowed
                        "RequestExcerptRegeneration"
                        "only proofread drafts can request excerpt regeneration"
                    )
