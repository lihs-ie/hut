module UseCase.CheckSlugAvailability (
    CheckSlugAvailabilityPayload (..),
    CheckSlugAvailabilityCommand,
    SlugAvailability (..),
    CheckSlugAvailabilityResult (..),
    Dependencies (..),
    checkSlugAvailability,
) where

import Data.Text (Text)
import Domain.Article (Article, articleIdentifier)
import Domain.Article.Common (ArticleIdentifier, articleIdentifierText)
import Shared.Domain.Error (DomainError, createAggregateNotFound, createUnexpectedError)
import Shared.Domain.Event (Events (..))
import Shared.Domain.Slug (Slug, newSlug)
import Shared.UseCase.Command (Command (..))
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

data CheckSlugAvailabilityPayload = CheckSlugAvailabilityPayload
    { article :: ArticleIdentifier
    , slug :: Text
    }
    deriving stock (Show, Eq)
type CheckSlugAvailabilityCommand = Command CheckSlugAvailabilityPayload

data SlugAvailability = Available | InUse
    deriving stock (Show, Eq)
data CheckSlugAvailabilityResult = CheckSlugAvailabilityResult
    { availability :: SlugAvailability
    , events :: Events (ArticleEventsFor 'Result.CheckSlugAvailability)
    }
data Dependencies m = Dependencies
    { findArticle :: ArticleIdentifier -> m (Either DomainError (Maybe Article))
    , -- Search ALL states; this is advisory, not a reservation.
      findSlugOwner :: Slug -> m (Either DomainError (Maybe ArticleIdentifier))
    }

checkSlugAvailability ::
    (Monad m) =>
    Dependencies m ->
    CheckSlugAvailabilityCommand ->
    m (Either DomainError CheckSlugAvailabilityResult)
checkSlugAvailability dependencies command = case newSlug command.payload.slug of
    Left err -> pure (Left err)
    Right slug -> do
        found <- dependencies.findArticle command.payload.article
        case found of
            Left err -> pure (Left err)
            Right Nothing -> pure (Left (createAggregateNotFound "Article" (articleIdentifierText command.payload.article)))
            Right (Just article)
                | articleIdentifier article /= command.payload.article ->
                    pure (Left (createUnexpectedError "Article" "loaded identity does not match request"))
                | otherwise -> do
                    owner <- dependencies.findSlugOwner slug
                    pure $ do
                        current <- owner
                        let availability = case current of
                                Just value | value /= command.payload.article -> InUse
                                _ -> Available
                        Right (CheckSlugAvailabilityResult availability (Events []))
