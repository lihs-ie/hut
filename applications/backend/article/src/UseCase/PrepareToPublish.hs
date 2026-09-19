module UseCase.PrepareToPublish (
    PrepareToPublishPayload (..),
    PrepareToPublishCommand,
    PrepareToPublishResult (..),
    Dependencies (..),
    prepareToPublish,
) where

import Data.Text (Text)
import Domain.Article (Article (..), articleIdentifier)
import Domain.Article.Common (ArticleIdentifier, articleIdentifierText)
import Domain.Article.Draft qualified as Draft
import Shared.Domain.Error (
    DomainError,
    createAggregateNotFound,
    createOperationNotAllowed,
    createUnexpectedError,
 )
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.Domain.Excerpt (newExcerpt)
import Shared.UseCase.Command (Command (..))
import UseCase.Persistence (LoadForPreparation, LoadedForPreparation (..), commandContext)
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

data Dependencies m = Dependencies
    { -- Bound by the consumer to the target revision from the event envelope.
      -- Reject stale revisions on load AND conditional save; never use "latest".
      loadGenerationTarget :: LoadForPreparation m
    , loadRevisionTarget :: LoadForPreparation m
    }

prepareToPublish ::
    (Monad m) =>
    Dependencies m ->
    PrepareToPublishCommand ->
    m (Either DomainError PrepareToPublishResult)
prepareToPublish dependencies command =
    case newExcerpt command.payload.excerpt of
        Left err -> pure (Left err)
        Right excerpt -> do
            loaded <- loadTarget command.payload.article
            case loaded of
                Left err -> pure (Left err)
                Right Nothing ->
                    pure
                        ( Left
                            ( createAggregateNotFound
                                "Article"
                                (articleIdentifierText command.payload.article)
                            )
                        )
                Right (Just snapshot)
                    | articleIdentifier snapshot.article /= command.payload.article ->
                        pure
                            ( Left
                                ( createUnexpectedError
                                    "Article"
                                    "loaded identity does not match request"
                                )
                            )
                    | otherwise ->
                        case transition excerpt snapshot.article of
                            Left err -> pure (Left err)
                            Right article -> do
                                let events = case command.payload of
                                        ApplyGeneratedExcerpt{} ->
                                            Events [Here (DomainEvent (Draft.draftIdentifier article))]
                                        ReviseExcerpt{} -> Events []
                                saved <- snapshot.savePreparation (commandContext command) article events
                                pure (PrepareToPublishResult article events <$ saved)
  where
    loadTarget = case command.payload of
        ApplyGeneratedExcerpt{} -> dependencies.loadGenerationTarget
        ReviseExcerpt{} -> dependencies.loadRevisionTarget
    transition excerpt article = case (command.payload, article) of
        (ApplyGeneratedExcerpt{}, Proofreaded draft) ->
            Draft.prepareToPublish command.timestamp excerpt draft
        (ReviseExcerpt{}, Ready draft) ->
            Draft.reviseExcerpt command.timestamp excerpt draft
        _ ->
            Left
                ( createOperationNotAllowed
                    "PrepareToPublish"
                    "generated excerpts require a proofread draft; revisions require a ready draft"
                )
