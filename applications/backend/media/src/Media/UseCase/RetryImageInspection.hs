module Media.UseCase.RetryImageInspection (
    RetryImageInspection,
    Dependencies (..),
    Error,
    Result,
    newRetryImageInspection,
    foldRetryImageInspection,
    retryImageInspection,
) where

import Media.Domain.Image (
    Image,
    ImageIdentifier,
    UploadAttemptIdentifier,
    imageIdentifierText,
    retryableInspectionAttempt,
 )
import Media.Internal.Result qualified as UseCase
import Shared.Domain.Error (DomainError, createAggregateNotFound)
import Shared.Domain.Event (Events (Events))
import Shared.UseCase.Command (Command (Command))

newtype RetryImageInspection = RetryImageInspection ImageIdentifier
    deriving stock (Show, Eq)

data Dependencies = Dependencies
    { findImage :: ImageIdentifier -> IO (Either DomainError (Maybe Image))
    , enqueueImageInspection :: UploadAttemptIdentifier -> IO (Either DomainError ())
    }

type Error = DomainError

type Result = UseCase.Result UseCase.RetryImageInspection UploadAttemptIdentifier

newRetryImageInspection :: ImageIdentifier -> RetryImageInspection
newRetryImageInspection = RetryImageInspection

foldRetryImageInspection ::
    (ImageIdentifier -> result) -> RetryImageInspection -> result
foldRetryImageInspection transform (RetryImageInspection identifier) = transform identifier

retryImageInspection ::
    Dependencies -> Command RetryImageInspection -> IO (Either Error Result)
retryImageInspection dependencies (Command (RetryImageInspection identifier) _ _ _ _) = do
    found <- dependencies.findImage identifier
    case found of
        Left err -> pure (Left err)
        Right Nothing -> pure (Left (imageNotFound identifier))
        Right (Just image) ->
            case retryableInspectionAttempt image of
                Left err -> pure (Left err)
                Right attempt -> do
                    enqueued <- dependencies.enqueueImageInspection attempt
                    pure (UseCase.newResult attempt (Events []) <$ enqueued)

imageNotFound :: ImageIdentifier -> DomainError
imageNotFound identifier =
    createAggregateNotFound "Image" (imageIdentifierText identifier)
