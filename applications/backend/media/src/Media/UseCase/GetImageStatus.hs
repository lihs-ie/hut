module Media.UseCase.GetImageStatus (
    GetImageStatus,
    Dependencies (..),
    Error,
    Result,
    newGetImageStatus,
    foldGetImageStatus,
    getImageStatus,
) where

import Media.Domain.Image (Image, ImageIdentifier, imageIdentifierText)
import Media.Internal.Result qualified as UseCase
import Shared.Domain.Error (DomainError, createAggregateNotFound)
import Shared.Domain.Event (Events (Events))
import Shared.UseCase.Command (Command (Command))

newtype GetImageStatus = GetImageStatus ImageIdentifier
    deriving stock (Show, Eq)

newtype Dependencies = Dependencies
    { findImage :: ImageIdentifier -> IO (Either DomainError (Maybe Image))
    }

type Error = DomainError

type Result = UseCase.Result UseCase.GetImageStatus Image

newGetImageStatus :: ImageIdentifier -> GetImageStatus
newGetImageStatus = GetImageStatus

foldGetImageStatus :: (ImageIdentifier -> result) -> GetImageStatus -> result
foldGetImageStatus transform (GetImageStatus identifier) = transform identifier

getImageStatus :: Dependencies -> Command GetImageStatus -> IO (Either Error Result)
getImageStatus dependencies (Command (GetImageStatus identifier) _ _ _ _) = do
    found <- dependencies.findImage identifier
    pure $ case found of
        Left err -> Left err
        Right Nothing ->
            Left (createAggregateNotFound "Image" (imageIdentifierText identifier))
        Right (Just image) -> Right (UseCase.newResult image (Events []))
