module Media.UseCase.ProcessImageInspection (
    TemporaryObjectKey,
    FinalObjectKey,
    InspectionClaim (..),
    ProcessImageInspection,
    InspectionNormalization (..),
    InspectionFailureRecord (..),
    InspectionDependencies (..),
    InspectionProcessingResult (..),
    newFinalObjectKey,
    newProcessImageInspection,
    foldProcessImageInspection,
    newTemporaryObjectKey,
    finalObjectKeyText,
    temporaryObjectKeyText,
    processUploadedImage,
    recordInspectionDLQFailure,
) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Media.Domain.Image (
    AwaitingUploadImage,
    ImageRejection,
    ImageUploadDeclaration,
    SuccessfulImageInspection,
    UploadAttemptIdentifier,
    foldAwaitingUploadImage,
    imageIdentifierText,
 )
import Media.UseCase.InspectImage (InspectImageResult, inspectImage, newAcceptedInspection)
import Media.UseCase.InspectImage qualified as Inspect
import Shared.UseCase.Command (Command (..))

newtype FinalObjectKey = FinalObjectKey Text
    deriving stock (Show, Eq)

newtype TemporaryObjectKey = TemporaryObjectKey Text
    deriving stock (Show, Eq)

data InspectionClaim = InspectionClaim
    { image :: AwaitingUploadImage
    , temporaryObjectKey :: TemporaryObjectKey
    }
    deriving stock (Show, Eq)

data ProcessImageInspection = ProcessImageInspection
    { uploadAttempt :: UploadAttemptIdentifier
    , uploadedAt :: UTCTime
    }
    deriving stock (Show, Eq)

data InspectionNormalization
    = ImageNormalized SuccessfulImageInspection
    | ImagePermanentlyRejected ImageRejection
    deriving stock (Show, Eq)

data InspectionFailureRecord = InspectionFailureRecord
    { uploadAttempt :: UploadAttemptIdentifier
    , code :: Text
    , detail :: Maybe Text
    , failedAt :: UTCTime
    }
    deriving stock (Show, Eq)

data InspectionDependencies = InspectionDependencies
    { claimCurrentUpload ::
        UploadAttemptIdentifier -> UTCTime -> UTCTime -> IO (Maybe InspectionClaim)
    , normalizeImage ::
        ImageUploadDeclaration ->
        TemporaryObjectKey ->
        FinalObjectKey ->
        IO InspectionNormalization
    , currentTime :: IO UTCTime
    , commitInspection ::
        Command UploadAttemptIdentifier ->
        InspectionNormalization ->
        InspectImageResult ->
        FinalObjectKey ->
        IO ()
    , deleteTemporaryObject :: TemporaryObjectKey -> IO ()
    , persistInspectionFailure :: InspectionFailureRecord -> IO ()
    }

data InspectionProcessingResult = StaleUploadIgnored | InspectionCommitted InspectImageResult
    deriving stock (Show, Eq)

newFinalObjectKey :: Text -> FinalObjectKey
newFinalObjectKey = FinalObjectKey

newProcessImageInspection ::
    UploadAttemptIdentifier -> UTCTime -> ProcessImageInspection
newProcessImageInspection = ProcessImageInspection

foldProcessImageInspection ::
    (UploadAttemptIdentifier -> UTCTime -> result) -> ProcessImageInspection -> result
foldProcessImageInspection transform (ProcessImageInspection attempt uploadedAt) =
    transform attempt uploadedAt

newTemporaryObjectKey :: Text -> TemporaryObjectKey
newTemporaryObjectKey = TemporaryObjectKey

finalObjectKeyText :: FinalObjectKey -> Text
finalObjectKeyText (FinalObjectKey value) = value

temporaryObjectKeyText :: TemporaryObjectKey -> Text
temporaryObjectKeyText (TemporaryObjectKey value) = value

processUploadedImage ::
    InspectionDependencies -> Command ProcessImageInspection -> IO InspectionProcessingResult
processUploadedImage dependencies command = do
    let ProcessImageInspection attempt uploadedAt = command.payload
    claimed <-
        dependencies.claimCurrentUpload
            attempt
            uploadedAt
            command.timestamp
    case claimed of
        Nothing -> pure StaleUploadIgnored
        Just claim -> do
            let (finalKey, declaration) = claimValues claim.image
            normalization <-
                dependencies.normalizeImage
                    declaration
                    claim.temporaryObjectKey
                    finalKey
            completedAt <- dependencies.currentTime
            let inspectionCommand =
                    Command
                        attempt
                        completedAt
                        command.actor
                        command.correlation
                        command.causation
            domainRequest <- case normalization of
                ImageNormalized evidence ->
                    pure
                        (newAcceptedInspection attempt claim.image evidence)
                ImagePermanentlyRejected rejection ->
                    pure (Inspect.newRejectedInspection attempt claim.image rejection)
            result <-
                either
                    (ioError . userError . show)
                    pure
                    ( inspectImage
                        ( Command
                            domainRequest
                            completedAt
                            command.actor
                            command.correlation
                            command.causation
                        )
                    )
            dependencies.commitInspection inspectionCommand normalization result finalKey
            dependencies.deleteTemporaryObject claim.temporaryObjectKey
            pure (InspectionCommitted result)
  where
    claimValues =
        foldAwaitingUploadImage
            ( \identifier _ declaration _ ->
                (FinalObjectKey ("images/" <> imageIdentifierText identifier), declaration)
            )

recordInspectionDLQFailure :: InspectionDependencies -> InspectionFailureRecord -> IO ()
recordInspectionDLQFailure = (.persistInspectionFailure)
