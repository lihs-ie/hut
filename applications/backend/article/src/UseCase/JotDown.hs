module UseCase.JotDown (
    JotDownPayload,
    JotDownCommand,
    JotDownResult (..),
    Dependencies (..),
    jotDown,
) where

import Domain.Article.Common (
    ArticleIdentifier,
    DraftInput,
    ExtractImageReferences,
    newDraftContent,
 )
import Domain.Article.Draft (UnvalidatedDraft, newUnvalidatedDraft)
import Domain.Article.Event (draftImageReferences)
import Shared.Domain.Error (DomainError)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.UseCase.Command (Command (..))
import UseCase.Persistence (SaveDraft, commandContext)
import UseCase.Result (ArticleEventsFor, ArticleUseCase (JotDown))

type JotDownPayload = DraftInput
type JotDownCommand = Command JotDownPayload

data JotDownResult = JotDownResult
    { article :: UnvalidatedDraft
    , events :: Events (ArticleEventsFor 'JotDown)
    }

data Dependencies m = Dependencies
    { newArticleIdentifier :: m (Either DomainError ArticleIdentifier)
    , extractImageReferences :: ExtractImageReferences
    , -- Insert-only, including identity collisions: never upsert an existing article.
      saveNewDraft :: SaveDraft m (ArticleEventsFor 'JotDown)
    }

jotDown :: (Monad m) => Dependencies m -> JotDownCommand -> m (Either DomainError JotDownResult)
jotDown dependencies command =
    case newDraftContent dependencies.extractImageReferences command.payload of
        Left err -> pure (Left err)
        Right content -> do
            generated <- dependencies.newArticleIdentifier
            case generated
                >>= ( \identifier ->
                        newUnvalidatedDraft identifier command.timestamp content
                    ) of
                Left err -> pure (Left err)
                Right article -> do
                    let events = Events [Here (DomainEvent (draftImageReferences article))]
                    saved <- dependencies.saveNewDraft (commandContext command) article events
                    pure (JotDownResult article events <$ saved)
