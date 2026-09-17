module Media.UseCase.GetImageStatusSpec (run) where

import Media.Domain.Image (
    Image (AwaitingUpload),
    imageIdentifierText,
 )
import Media.TestSupport (
    assertEqual,
    assertLeftEqual,
    baseTime,
    expectRight,
    hasNoEvents,
    testAwaiting,
    testCommandAt,
    testImageIdentifier,
    testUploadAttemptIdentifier,
    (<&&>),
 )
import Media.UseCase.GetImageStatus qualified as GetStatus (
    Dependencies (Dependencies),
    getImageStatus,
    newGetImageStatus,
 )
import Media.UseCase.RequestImageUpload qualified as RequestUpload (

 )
import Media.UseCase.Result qualified as UseCase (
    resultEvents,
    resultOutput,
 )
import Media.UseCase.RetryImageInspection qualified as RetryInspection (

 )
import Shared.Domain.Error (
    createAggregateNotFound,
    createInvariantViolation,
 )

run :: IO Bool
run =
    and
        <$> sequence
            [ testGetImageStatus
            , testImageStatusNotFound
            , testGetImageStatusRepositoryFailure
            ]

testGetImageStatus :: IO Bool
testGetImageStatus = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    command <- testCommandAt baseTime (GetStatus.newGetImageStatus imageIdentifier)
    let image = AwaitingUpload awaiting
        dependencies = GetStatus.Dependencies (\_ -> pure (Right (Just image)))
    result <- GetStatus.getImageStatus dependencies command >>= expectRight "get status"
    assertEqual "status returns the image aggregate" image (UseCase.resultOutput result)
        <&&> assertEqual "status returns no events" True (hasNoEvents (UseCase.resultEvents result))

testImageStatusNotFound :: IO Bool
testImageStatusNotFound = do
    imageIdentifier <- testImageIdentifier 1
    command <- testCommandAt baseTime (GetStatus.newGetImageStatus imageIdentifier)
    let dependencies = GetStatus.Dependencies (\_ -> pure (Right Nothing))
        expected = createAggregateNotFound "Image" (imageIdentifierText imageIdentifier)
    result <- GetStatus.getImageStatus dependencies command
    assertLeftEqual "missing image is AggregateNotFound" expected result

testGetImageStatusRepositoryFailure :: IO Bool
testGetImageStatusRepositoryFailure = do
    imageIdentifier <- testImageIdentifier 1
    command <- testCommandAt baseTime (GetStatus.newGetImageStatus imageIdentifier)
    let expected = createInvariantViolation "Image" "lookup failed"
        dependencies = GetStatus.Dependencies (\_ -> pure (Left expected))
    result <- GetStatus.getImageStatus dependencies command
    assertLeftEqual "status returns repository failure unchanged" expected result
