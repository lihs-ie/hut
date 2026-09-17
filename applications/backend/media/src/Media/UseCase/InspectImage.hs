module Media.UseCase.InspectImage (
    InspectImage,
    InspectImageResult,
    InspectImageError,
    newAcceptedInspection,
    newUnsupportedImageInspection,
    newMalformedImageInspection,
    newRejectedInspection,
    inspectImage,
    inspectImageResult,
    foldInspectImageResult,
) where

import Data.Time (UTCTime)
import Media.Domain.Image (
    AvailableImage,
    AwaitingUploadImage,
    Image (Available, Rejected),
    ImageRejection (MalformedImage, UnsupportedImageFormat),
    InspectingImage,
    RejectedImageUpload,
    SuccessfulImageInspection,
    UploadAttemptIdentifier,
    acceptImageInspection,
    beginImageInspection,
    foldRejectedImageUpload,
    rejectImageInspection,
 )
import Media.Domain.Image.Event (
    ImageBecameAvailable,
    ImageUploadRejected,
    newImageBecameAvailable,
    newImageUploadRejected,
 )
import Media.Internal.Result qualified as UseCase
import Shared.Domain.Error (DomainError)
import Shared.Domain.Event (Events (Events), OneOf (Here, There))
import Shared.UseCase.Command (Command (Command))

data InspectImage = InspectImage
    { image :: AwaitingUploadImage
    , uploadAttempt :: UploadAttemptIdentifier
    , outcome :: InspectionOutcome
    }
    deriving stock (Show, Eq)

data InspectionOutcome
    = InspectionSucceeded SuccessfulImageInspection
    | InspectionRejected ImageRejection
    deriving stock (Show, Eq)

data InspectImageResult
    = ImageAcceptedResult AvailableImage ImageBecameAvailable
    | ImageRejectedResult RejectedImageUpload ImageUploadRejected
    deriving stock (Show, Eq)

type InspectImageError = DomainError

newAcceptedInspection ::
    UploadAttemptIdentifier -> AwaitingUploadImage -> SuccessfulImageInspection -> InspectImage
newAcceptedInspection attempt awaiting successful =
    InspectImage awaiting attempt (InspectionSucceeded successful)

newUnsupportedImageInspection :: UploadAttemptIdentifier -> AwaitingUploadImage -> InspectImage
newUnsupportedImageInspection attempt awaiting =
    InspectImage awaiting attempt (InspectionRejected UnsupportedImageFormat)

newMalformedImageInspection :: UploadAttemptIdentifier -> AwaitingUploadImage -> InspectImage
newMalformedImageInspection attempt awaiting =
    InspectImage awaiting attempt (InspectionRejected MalformedImage)

newRejectedInspection ::
    UploadAttemptIdentifier -> AwaitingUploadImage -> ImageRejection -> InspectImage
newRejectedInspection attempt awaiting reason =
    InspectImage awaiting attempt (InspectionRejected reason)

inspectImage :: Command InspectImage -> Either InspectImageError InspectImageResult
inspectImage (Command request completedAt _ _ _) = do
    inspecting <- beginImageInspection completedAt request.uploadAttempt request.image
    pure (completeImageInspection completedAt inspecting request.outcome)

completeImageInspection :: UTCTime -> InspectingImage -> InspectionOutcome -> InspectImageResult
completeImageInspection completedAt inspecting inspectionResult =
    case inspectionResult of
        InspectionSucceeded inspected ->
            case acceptImageInspection completedAt inspecting inspected of
                Left reason -> rejectedResult completedAt reason inspecting
                Right available -> ImageAcceptedResult available (newImageBecameAvailable available)
        InspectionRejected reason ->
            rejectedResult completedAt reason inspecting

rejectedResult :: UTCTime -> ImageRejection -> InspectingImage -> InspectImageResult
rejectedResult completedAt reason inspecting =
    let rejected = rejectImageInspection completedAt reason inspecting
        imageIdentifier =
            foldRejectedImageUpload
                (\identifier _ _ -> identifier)
                rejected
     in ImageRejectedResult rejected (newImageUploadRejected imageIdentifier reason)

inspectImageResult :: InspectImageResult -> UseCase.Result UseCase.InspectImage Image
inspectImageResult (ImageAcceptedResult available event) =
    UseCase.newResult (Available available) (Events [Here event])
inspectImageResult (ImageRejectedResult rejected event) =
    UseCase.newResult (Rejected rejected) (Events [There (Here event)])

foldInspectImageResult ::
    (AvailableImage -> ImageBecameAvailable -> result) ->
    (RejectedImageUpload -> ImageUploadRejected -> result) ->
    InspectImageResult ->
    result
foldInspectImageResult onAccepted _ (ImageAcceptedResult available event) =
    onAccepted available event
foldInspectImageResult _ onRejected (ImageRejectedResult rejected event) = onRejected rejected event
