module UseCase.AmendDraft (
    AmendDraftPayload (..),
    AmendDraftCommand,
    AmendDraftResult (..),
    Dependencies (..),
    amendDraft,
) where

import Data.Text (Text)
import Domain.Article
import Domain.Article.Draft qualified as Draft
import Domain.Article.Event (draftImageReferences)
import Shared.Domain.Common.Transaction (Transaction, TransactionManager, fromEither, runTransaction)
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

data AmendDraftPayload = AmendDraftPayload
    { article :: ArticleIdentifier
    , title :: Text
    , body :: Text
    , slug :: Maybe Text
    , tags :: [Text]
    }
    deriving stock (Show, Eq)

type AmendDraftCommand = Command AmendDraftPayload

data AmendDraftResult = AmendDraftResult
    { article :: Draft.UnvalidatedDraft
    , events :: Events (ArticleEventsFor 'Result.AmendDraft)
    }

data Dependencies context m = Dependencies
    { transactionManager :: TransactionManager context m
    , findArticle :: FindArticle (Transaction context m)
    , persistArticle :: PersistArticle (Transaction context m)
    , appendEvents :: Append (ArticleEventsFor 'Result.AmendDraft) (Transaction context m)
    , extractImageReferences :: ExtractImageReferences
    }

amendDraft :: (Monad m) => Dependencies context m -> AmendDraftCommand -> m (Either DomainError AmendDraftResult)
amendDraft dependencies command = runTransaction dependencies.transactionManager $ do
    source <- requireArticle dependencies.findArticle command.payload.article
    article <- fromEither (amend source)
    let events = Events [Here (DomainEvent (draftImageReferences article))]
    dependencies.persistArticle (Unvalidated article)
    dependencies.appendEvents (commandContext command) events
    pure (AmendDraftResult article events)
  where
    input = DraftInput command.payload.title command.payload.body command.payload.slug command.payload.tags
    amend article = case article of
        Unvalidated draft -> apply draft
        Proofreaded draft -> apply draft
        Ready draft -> apply draft
        Published _ -> Left notEditable
        Private _ -> Left notEditable
    apply :: Draft.Draft phase -> Either DomainError Draft.UnvalidatedDraft
    apply draft = do
        content <- newDraftContent dependencies.extractImageReferences input
        Draft.amendDraft command.timestamp content draft
    notEditable =
        createOperationNotAllowed
            "AmendDraft"
            "only drafts can be amended; take down and resume publication first"
