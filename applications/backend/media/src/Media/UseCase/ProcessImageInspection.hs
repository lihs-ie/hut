module Media.UseCase.ProcessImageInspection (
    TemporaryObjectKey,
    FinalObjectKey,
    InspectionClaim (..),
    InspectionNormalization (..),
    InspectionFailureRecord (..),
    InspectionDependencies (..),
    InspectionProcessingResult (..),
    newFinalObjectKey,
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
    { claimCurrentUpload :: UploadAttemptIdentifier -> UTCTime -> IO (Maybe InspectionClaim)
    , normalizeImage :: TemporaryObjectKey -> FinalObjectKey -> IO InspectionNormalization
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

newTemporaryObjectKey :: Text -> TemporaryObjectKey
newTemporaryObjectKey = TemporaryObjectKey

finalObjectKeyText :: FinalObjectKey -> Text
finalObjectKeyText (FinalObjectKey value) = value

temporaryObjectKeyText :: TemporaryObjectKey -> Text
temporaryObjectKeyText (TemporaryObjectKey value) = value

processUploadedImage ::
    InspectionDependencies -> Command UploadAttemptIdentifier -> IO InspectionProcessingResult
processUploadedImage dependencies command = do
    claimed <- dependencies.claimCurrentUpload command.payload command.timestamp
    case claimed of
        Nothing -> pure StaleUploadIgnored
        Just claim -> do
            let finalKey = keyFor claim.image
            normalization <- dependencies.normalizeImage claim.temporaryObjectKey finalKey
            domainRequest <- case normalization of
                ImageNormalized evidence ->
                    pure
                        (newAcceptedInspection command.payload claim.image evidence)
                ImagePermanentlyRejected rejection ->
                    pure (Inspect.newRejectedInspection command.payload claim.image rejection)
            result <-
                either
                    (ioError . userError . show)
                    pure
                    (inspectImage command{payload = domainRequest})
            dependencies.commitInspection command normalization result finalKey
            dependencies.deleteTemporaryObject claim.temporaryObjectKey
            pure (InspectionCommitted result)
  where
    keyFor =
        foldAwaitingUploadImage
            (\identifier _ _ _ -> FinalObjectKey ("images/" <> imageIdentifierText identifier))

recordInspectionDLQFailure :: InspectionDependencies -> InspectionFailureRecord -> IO ()
recordInspectionDLQFailure = (.persistInspectionFailure)
