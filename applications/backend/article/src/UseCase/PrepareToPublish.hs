module UseCase.PrepareToPublish (
    PrepareToPublishPayload (..),
    PrepareToPublishCommand,
    PrepareToPublishResult (..),
    Dependencies (..),
    prepareToPublish,
) where

import Data.Text (Text)
import Domain.Article
import Domain.Article.Draft qualified as Draft
import Shared.Domain.Common.Transaction (Transaction, TransactionManager, fromEither, runTransaction)
import Shared.Domain.Error (DomainError, createOperationNotAllowed)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.Domain.Excerpt (newExcerpt)
import Shared.UseCase.Command (Command (..), commandContext)
import Shared.UseCase.Outbox (Append)
import UseCase.Helper
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

-- Origin is a use-case concern, not a domain state.
-- The generated command is constructed only by the authenticated event consumer.
data PrepareToPublishPayload
    = ApplyGeneratedExcerpt {article :: ArticleIdentifier, excerpt :: Text}
    | ReviseExcerpt {article :: ArticleIdentifier, excerpt :: Text}
    deriving stock (Show, Eq)

type PrepareToPublishCommand = Command PrepareToPublishPayload

data PrepareToPublishResult = PrepareToPublishResult
    { article :: Draft.ReadyToPublish
    , events :: Events (ArticleEventsFor 'Result.PrepareToPublish)
    }

data Dependencies context m = Dependencies
    { transactionManager :: TransactionManager context m
    , findArticle :: FindArticle (Transaction context m)
    , persistArticle :: PersistArticle (Transaction context m)
    , appendEvents :: Append (ArticleEventsFor 'Result.PrepareToPublish) (Transaction context m)
    }

prepareToPublish ::
    (Monad m) =>
    Dependencies context m ->
    PrepareToPublishCommand ->
    m (Either DomainError PrepareToPublishResult)
prepareToPublish dependencies command = runTransaction dependencies.transactionManager $ do
    excerpt <- fromEither (newExcerpt command.payload.excerpt)
    source <- requireArticle dependencies.findArticle command.payload.article
    article <- fromEither (transition excerpt source)
    let events = case command.payload of
            ApplyGeneratedExcerpt{} -> Events [Here (DomainEvent (Draft.draftIdentifier article))]
            ReviseExcerpt{} -> Events []
    dependencies.persistArticle (Ready article)
    case command.payload of
        ApplyGeneratedExcerpt{} -> dependencies.appendEvents (commandContext command) events
        ReviseExcerpt{} -> pure ()
    pure (PrepareToPublishResult article events)
  where
    transition excerpt article = case (command.payload, article) of
        (ApplyGeneratedExcerpt{}, Proofreaded draft) -> Draft.prepareToPublish command.timestamp excerpt draft
        (ReviseExcerpt{}, Ready draft) -> Draft.reviseExcerpt command.timestamp excerpt draft
        _ ->
            Left
                ( createOperationNotAllowed
                    "PrepareToPublish"
                    "generated excerpts require a proofread draft; revisions require a ready draft"
                )
