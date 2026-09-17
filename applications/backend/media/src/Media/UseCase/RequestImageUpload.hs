module Media.UseCase.RequestImageUpload (
    RequestImageUpload,
    Dependencies (..),
    Error,
    Result,
    UploadDestination,
    UploadDestinationURL,
    newRequestImageUpload,
    newUploadDestination,
    newUploadDestinationURL,
    foldRequestImageUpload,
    foldUploadDestination,
    uploadDestinationURLText,
    requestImageUpload,
) where

import Media.Domain.Image (
    AwaitingUploadImage,
    DeclaredImageContentType,
    Image (AwaitingUpload),
    ImageByteSize,
    ImageIdentifier,
    ImageSha256,
    UploadAttemptIdentifier,
    newAwaitingUploadImage,
    newImageUploadDeclaration,
 )
import Media.Internal.Result qualified as UseCase
import Media.Internal.UploadDestination
import Shared.Domain.Error (DomainError)
import Shared.Domain.Event (Events (Events))
import Shared.UseCase.Command (Command (Command))

data RequestImageUpload = RequestImageUpload
    { contentType :: DeclaredImageContentType
    , byteSize :: ImageByteSize
    , sha256 :: ImageSha256
    }
    deriving stock (Show, Eq)

data Dependencies = Dependencies
    { newImageIdentifier :: IO (Either DomainError ImageIdentifier)
    , newUploadAttemptIdentifier :: IO (Either DomainError UploadAttemptIdentifier)
    , persistImage :: Image -> IO (Either DomainError ())
    , issueUploadDestination ::
        AwaitingUploadImage -> IO (Either DomainError UploadDestination)
    }

type Error = DomainError

type Result = UseCase.Result UseCase.RequestImageUpload UploadDestination

newRequestImageUpload ::
    DeclaredImageContentType -> ImageByteSize -> ImageSha256 -> RequestImageUpload
newRequestImageUpload = RequestImageUpload

foldRequestImageUpload ::
    (DeclaredImageContentType -> ImageByteSize -> ImageSha256 -> result) ->
    RequestImageUpload ->
    result
foldRequestImageUpload transform (RequestImageUpload contentType byteSize sha256) =
    transform contentType byteSize sha256

requestImageUpload :: Dependencies -> Command RequestImageUpload -> IO (Either Error Result)
requestImageUpload dependencies (Command request requestedAt _ _ _) = do
    imageIdentifierResult <- dependencies.newImageIdentifier
    case imageIdentifierResult of
        Left err -> pure (Left err)
        Right imageIdentifier -> do
            uploadAttemptResult <- dependencies.newUploadAttemptIdentifier
            case uploadAttemptResult of
                Left err -> pure (Left err)
                Right uploadAttempt -> do
                    let declaration =
                            foldRequestImageUpload newImageUploadDeclaration request
                        awaiting =
                            newAwaitingUploadImage
                                imageIdentifier
                                uploadAttempt
                                declaration
                                requestedAt
                    persisted <- dependencies.persistImage (AwaitingUpload awaiting)
                    case persisted of
                        Left err -> pure (Left err)
                        Right () -> do
                            destination <- dependencies.issueUploadDestination awaiting
                            pure
                                ( UseCase.newResult
                                    <$> destination
                                    <*> pure (Events [])
                                )
