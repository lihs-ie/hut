module Media.UseCase.FindAvailableImages (
    FindAvailableImages,
    Dependencies (..),
    Error,
    Result,
    newFindAvailableImages,
    findAvailableImages,
) where

import Media.Domain.Image (ImageIdentifier)
import Media.Internal.Result qualified as UseCase
import Shared.Domain.Error (DomainError)
import Shared.Domain.Event (Events (Events))
import Shared.UseCase.Command (Command (Command))

newtype FindAvailableImages = FindAvailableImages [ImageIdentifier]
    deriving stock (Show, Eq)

newtype Dependencies = Dependencies
    { findAvailable :: [ImageIdentifier] -> IO (Either DomainError [ImageIdentifier])
    }

type Error = DomainError

type Result = UseCase.Result UseCase.FindAvailableImages [ImageIdentifier]

newFindAvailableImages :: [ImageIdentifier] -> FindAvailableImages
newFindAvailableImages = FindAvailableImages

findAvailableImages :: Dependencies -> Command FindAvailableImages -> IO (Either Error Result)
findAvailableImages dependencies (Command (FindAvailableImages identifiers) _ _ _ _) =
    fmap (\available -> UseCase.newResult available (Events []))
        <$> dependencies.findAvailable identifiers
