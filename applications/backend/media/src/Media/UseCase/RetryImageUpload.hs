module Media.UseCase.RetryImageUpload (
    RetryImageUpload,
    Dependencies (..),
    Error,
    Result,
    UploadDestination,
    UploadDestinationURL,
    newRetryImageUpload,
    newUploadDestination,
    newUploadDestinationURL,
    foldRetryImageUpload,
    foldUploadDestination,
    uploadDestinationURLText,
    retryImageUpload,
) where

import Media.Domain.Image (
    AwaitingUploadImage,
    Image (AwaitingUpload),
    ImageIdentifier,
    UploadAttemptIdentifier,
    imageIdentifierText,
    restartImageUpload,
 )
import Media.Internal.Result qualified as UseCase
import Media.Internal.UploadDestination
import Shared.Domain.Error (DomainError, createAggregateNotFound)
import Shared.Domain.Event (Events (Events))
import Shared.UseCase.Command (Command (Command))

newtype RetryImageUpload = RetryImageUpload ImageIdentifier
    deriving stock (Show, Eq)

data Dependencies = Dependencies
    { findImage :: ImageIdentifier -> IO (Either DomainError (Maybe Image))
    , newUploadAttemptIdentifier :: IO (Either DomainError UploadAttemptIdentifier)
    , persistImage :: Image -> IO (Either DomainError ())
    , issueUploadDestination ::
        AwaitingUploadImage -> IO (Either DomainError UploadDestination)
    }

type Error = DomainError

type Result = UseCase.Result UseCase.RetryImageUpload UploadDestination

newRetryImageUpload :: ImageIdentifier -> RetryImageUpload
newRetryImageUpload = RetryImageUpload

foldRetryImageUpload :: (ImageIdentifier -> result) -> RetryImageUpload -> result
foldRetryImageUpload transform (RetryImageUpload identifier) = transform identifier

retryImageUpload :: Dependencies -> Command RetryImageUpload -> IO (Either Error Result)
retryImageUpload dependencies (Command (RetryImageUpload identifier) requestedAt _ _ _) = do
    found <- dependencies.findImage identifier
    case found of
        Left err -> pure (Left err)
        Right Nothing -> pure (Left (imageNotFound identifier))
        Right (Just image) -> do
            attemptResult <- dependencies.newUploadAttemptIdentifier
            case attemptResult of
                Left err -> pure (Left err)
                Right attempt ->
                    case restartImageUpload attempt requestedAt image of
                        Left err -> pure (Left err)
                        Right awaiting -> do
                            persisted <- dependencies.persistImage (AwaitingUpload awaiting)
                            case persisted of
                                Left err -> pure (Left err)
                                Right () -> do
                                    destination <-
                                        dependencies.issueUploadDestination awaiting
                                    pure
                                        ( UseCase.newResult
                                            <$> destination
                                            <*> pure (Events [])
                                        )

imageNotFound :: ImageIdentifier -> DomainError
imageNotFound identifier =
    createAggregateNotFound "Image" (imageIdentifierText identifier)
