module Media.Domain.Image.RetentionSpec (run) where

import Data.Time (addUTCTime)
import Media.Domain.Image.Retention (
    awaitingUploadRetention,
    rejectedImageRetention,
    shouldDeleteAwaitingUpload,
    shouldDeleteRejectedImage,
    shouldDeleteUnreferencedAvailableImage,
    unreferencedAvailableImageRetention,
 )
import Media.TestSupport (
    acceptedImage,
    assertEqual,
    baseTime,
    rejectedImage,
    testAwaiting,
    testImageIdentifier,
    testUploadAttemptIdentifier,
    (<&&>),
 )
import Media.UseCase.RequestImageUpload qualified as RequestUpload (

 )
import Media.UseCase.RetryImageInspection qualified as RetryInspection (

 )

run :: IO Bool
run =
    and
        <$> sequence
            [ testRetentionBoundaries
            ]

testRetentionBoundaries :: IO Bool
testRetentionBoundaries = do
    accepted <- acceptedImage
    rejected <- rejectedImage
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    assertEqual
        "awaiting is retained before 24 hours"
        False
        (shouldDeleteAwaitingUpload (addUTCTime (awaitingUploadRetention - 1) baseTime) awaiting)
        <&&> assertEqual
            "awaiting is deleted at 24 hours"
            True
            (shouldDeleteAwaitingUpload (addUTCTime awaitingUploadRetention baseTime) awaiting)
        <&&> assertEqual
            "rejected is retained before 7 days"
            False
            (shouldDeleteRejectedImage (addUTCTime (rejectedImageRetention - 1) baseTime) rejected)
        <&&> assertEqual
            "rejected is deleted at 7 days"
            True
            (shouldDeleteRejectedImage (addUTCTime rejectedImageRetention baseTime) rejected)
        <&&> assertEqual
            "unreferenced available image is retained before 30 days"
            False
            ( shouldDeleteUnreferencedAvailableImage
                (addUTCTime (unreferencedAvailableImageRetention - 1) baseTime)
                baseTime
                accepted
            )
        <&&> assertEqual
            "unreferenced available image is deleted at 30 days"
            True
            ( shouldDeleteUnreferencedAvailableImage
                (addUTCTime unreferencedAvailableImageRetention baseTime)
                baseTime
                accepted
            )
