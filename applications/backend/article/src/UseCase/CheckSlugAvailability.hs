module UseCase.CheckSlugAvailability (
    CheckSlugAvailabilityPayload (..),
    CheckSlugAvailabilityCommand,
    SlugAvailability (..),
    CheckSlugAvailabilityResult (..),
    Dependencies (..),
    checkSlugAvailability,
) where

import Data.Text (Text)
import Domain.Article
import Shared.Domain.Common.Transaction (Transaction, TransactionManager, fromEither, runTransaction)
import Shared.Domain.Error (DomainError)
import Shared.Domain.Event (Events (..))
import Shared.Domain.Slug (newSlug)
import Shared.UseCase.Command (Command (..))
import UseCase.Helper
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
data Dependencies context m = Dependencies
    { transactionManager :: TransactionManager context m
    , findArticle :: FindArticle (Transaction context m)
    , findSlugOwner :: FindSlugOwner (Transaction context m)
    }

checkSlugAvailability ::
    (Monad m) =>
    Dependencies context m ->
    CheckSlugAvailabilityCommand ->
    m (Either DomainError CheckSlugAvailabilityResult)
checkSlugAvailability dependencies command = runTransaction dependencies.transactionManager $ do
    slug <- fromEither (newSlug command.payload.slug)
    _ <- requireArticle dependencies.findArticle command.payload.article
    owner <- dependencies.findSlugOwner slug
    let availability = case owner of
            Just value | value /= command.payload.article -> InUse
            _ -> Available
    pure (CheckSlugAvailabilityResult availability (Events []))
