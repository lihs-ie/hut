module UseCase.Proofread (
    ProofreadPayload (..),
    ProofreadCommand,
    ProofreadResult (..),
    Dependencies (..),
    proofread,
) where

import Data.Set (Set)
import Data.Set qualified as Set
import Domain.Article
import Domain.Article.Draft qualified as Draft
import Shared.Domain.Common.Transaction (Transaction, TransactionManager, abort, fromEither, runTransaction)
import Shared.Domain.Error (
    DomainError,
    createOperationNotAllowed,
 )
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.UseCase.Command (Command (..), commandContext)
import Shared.UseCase.Outbox (Append)
import UseCase.Helper
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

newtype ProofreadPayload = ProofreadPayload {article :: ArticleIdentifier}
    deriving stock (Show, Eq)

type ProofreadCommand = Command ProofreadPayload

data ProofreadResult = ProofreadResult
    { article :: Draft.ProofreadedDraft
    , events :: Events (ArticleEventsFor 'Result.Proofread)
    }

data Dependencies context m = Dependencies
    { transactionManager :: TransactionManager context m
    , findArticle :: FindArticle (Transaction context m)
    , persistArticle :: PersistArticle (Transaction context m)
    , appendEvents :: Append (ArticleEventsFor 'Result.Proofread) (Transaction context m)
    , findAvailableImages :: Set ImageReference -> m (Either DomainError (Set ImageReference))
    }

proofread :: (Monad m) => Dependencies context m -> ProofreadCommand -> m (Either DomainError ProofreadResult)
proofread dependencies command = do
    preview <- runTransaction dependencies.transactionManager loadDraft
    case preview of
        Left err -> pure (Left err)
        Right draft -> do
            let requested = (Draft.draftContent draft).images
            availability <-
                if Set.null requested
                    then pure (Right Set.empty)
                    else dependencies.findAvailableImages requested
            case availability >>= confirmAvailableImageReferences requested of
                Left err -> pure (Left err)
                Right available -> runTransaction dependencies.transactionManager $ do
                    current <- loadDraft
                    if (Draft.draftContent current).images /= requested
                        then
                            abort
                                ( createOperationNotAllowed
                                    "Proofread"
                                    "image references changed during availability check"
                                )
                        else pure ()
                    article <- fromEither (Draft.proofread command.timestamp available current)
                    let events = Events [Here (DomainEvent (Draft.draftIdentifier article))]
                    dependencies.persistArticle (Proofreaded article)
                    dependencies.appendEvents (commandContext command) events
                    pure (ProofreadResult article events)
  where
    loadDraft = do
        article <- requireArticle dependencies.findArticle command.payload.article
        case article of
            Unvalidated draft -> pure draft
            _ -> abort (createOperationNotAllowed "Proofread" "only unvalidated drafts can be proofread")
