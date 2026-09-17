module Media.Domain.Image.Retention (
    awaitingUploadRetention,
    rejectedImageRetention,
    unreferencedAvailableImageRetention,
    shouldDeleteAwaitingUpload,
    shouldDeleteRejectedImage,
    shouldDeleteUnreferencedAvailableImage,
) where

import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import Media.Domain.Image (
    AvailableImage,
    AwaitingUploadImage,
    RejectedImageUpload,
    foldAwaitingUploadImage,
    foldRejectedImageUpload,
 )

awaitingUploadRetention :: NominalDiffTime
awaitingUploadRetention = 24 * 60 * 60

rejectedImageRetention :: NominalDiffTime
rejectedImageRetention = 7 * 24 * 60 * 60

unreferencedAvailableImageRetention :: NominalDiffTime
unreferencedAvailableImageRetention = 30 * 24 * 60 * 60

shouldDeleteAwaitingUpload :: UTCTime -> AwaitingUploadImage -> Bool
shouldDeleteAwaitingUpload now =
    foldAwaitingUploadImage
        (\_ _ _ requestedAt -> elapsedAtLeast awaitingUploadRetention requestedAt now)

shouldDeleteRejectedImage :: UTCTime -> RejectedImageUpload -> Bool
shouldDeleteRejectedImage now =
    foldRejectedImageUpload
        (\_ _ rejectedAt -> elapsedAtLeast rejectedImageRetention rejectedAt now)

shouldDeleteUnreferencedAvailableImage :: UTCTime -> UTCTime -> AvailableImage -> Bool
shouldDeleteUnreferencedAvailableImage now becameUnreferencedAt _ =
    elapsedAtLeast unreferencedAvailableImageRetention becameUnreferencedAt now

elapsedAtLeast :: NominalDiffTime -> UTCTime -> UTCTime -> Bool
elapsedAtLeast retention startedAt now = diffUTCTime now startedAt >= retention
