module Media.Presentation.Handler.InspectionQueueSpec (run) where

import Cloudflare.Workers.Entrypoint.Queue (
    QueueRetryOptions (QueueRetryOptions),
 )
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Media.Presentation.Handler.QueueTestSupport (
    commandMetadataMatches,
    emptyInspectionDependencies,
    fixedTime,
    inspectionBody,
    isLeft,
    named,
    newDisposition,
    r2Body,
    readDisposition,
    runInspection,
    temporaryKey,
    testAwaitingImage,
    testInspectionEvidence,
    testUploadAttemptIdentifier,
    validMessageID,
 )
import "media" Media.UseCase.ProcessImageInspection (
    InspectionClaim (InspectionClaim),
    InspectionDependencies (
        InspectionDependencies,
        claimCurrentUpload,
        persistInspectionFailure
    ),
    InspectionFailureRecord (code, detail, failedAt, uploadAttempt),
    InspectionNormalization (ImageNormalized),
    finalObjectKeyText,
    temporaryObjectKeyText,
 )

run :: IO Bool
run =
    and
        <$> sequence
            [ named "inspection queue success" inspectionSuccess
            , named "inspection dependency failure retries" inspectionDependencyFailure
            , named "stale inspection upload acknowledges" inspectionStaleUpload
            , named "invalid inspection messages acknowledge" inspectionInvalidInputs
            , named "inspection DLQ records failure" inspectionDLQ
            , named "inspection DLQ failure raises" inspectionDLQFailure
            ]

inspectionSuccess :: IO Bool
inspectionSuccess = do
    attempt <- testUploadAttemptIdentifier 2
    image <- testAwaitingImage attempt
    evidence <- testInspectionEvidence
    calls <- newIORef ([] :: [Text])
    capturedCommand <- newIORef Nothing
    capturedKeys <- newIORef Nothing
    disposition <- newDisposition
    let dependencies =
            InspectionDependencies
                ( \actualAttempt actualTime -> do
                    modifyIORef' calls (<> ["claim"])
                    if actualAttempt == attempt && actualTime == fixedTime
                        then pure (Just (InspectionClaim image temporaryKey))
                        else pure Nothing
                )
                ( \temporary final -> do
                    modifyIORef' calls (<> ["normalize"])
                    writeIORef
                        capturedKeys
                        (Just (temporaryObjectKeyText temporary, finalObjectKeyText final))
                    pure (ImageNormalized evidence)
                )
                ( \command _ _ _ -> do
                    modifyIORef' calls (<> ["commit"])
                    writeIORef capturedCommand (Just command)
                )
                (\_ -> modifyIORef' calls (<> ["delete"]))
                (\_ -> error "DLQ persistence must not run")
    runInspection
        "media-inspection"
        dependencies
        disposition
        (inspectionBody attempt)
        "opaque-cloudflare-message"
    actualCalls <- readIORef calls
    command <- readIORef capturedCommand
    keys <- readIORef capturedKeys
    (acked, retries) <- readDisposition disposition
    pure
        ( actualCalls == ["claim", "normalize", "commit", "delete"]
            && commandMetadataMatches attempt command
            && keys == Just ("tmp/upload", "images/00000000000000000000000001")
            && acked == 1
            && retries == []
        )

inspectionDependencyFailure :: IO Bool
inspectionDependencyFailure = do
    attempt <- testUploadAttemptIdentifier 2
    disposition <- newDisposition
    let dependencies =
            emptyInspectionDependencies
                { claimCurrentUpload = \_ _ -> throwIO (userError "D1 unavailable")
                }
    runInspection
        "media-inspection"
        dependencies
        disposition
        (inspectionBody attempt)
        validMessageID
    (acked, retries) <- readDisposition disposition
    pure (acked == 0 && retries == [QueueRetryOptions Nothing])

inspectionStaleUpload :: IO Bool
inspectionStaleUpload = do
    attempt <- testUploadAttemptIdentifier 2
    disposition <- newDisposition
    runInspection
        "media-inspection"
        emptyInspectionDependencies
        disposition
        (inspectionBody attempt)
        validMessageID
    (acked, retries) <- readDisposition disposition
    pure (acked == 1 && retries == [])

inspectionInvalidInputs :: IO Bool
inspectionInvalidInputs = do
    forM
        [ ("not-json", validMessageID)
        , (r2Body "tmp/not-a-ulid", validMessageID)
        ]
        ( \(body, messageID) -> do
            disposition <- newDisposition
            outcome <-
                try @SomeException
                    ( runInspection
                        "media-inspection"
                        emptyInspectionDependencies
                        disposition
                        body
                        messageID
                    )
            (acked, retries) <- readDisposition disposition
            pure (isLeft outcome && acked == 0 && retries == [])
        )
        >>= pure . and

inspectionDLQ :: IO Bool
inspectionDLQ = do
    attempt <- testUploadAttemptIdentifier 2
    captured <- newIORef Nothing
    disposition <- newDisposition
    let dependencies =
            emptyInspectionDependencies
                { persistInspectionFailure = writeIORef captured . Just
                }
    runInspection
        "MEDIA-INSPECTION-DLQ"
        dependencies
        disposition
        (inspectionBody attempt)
        validMessageID
    failure <- readIORef captured
    (acked, retries) <- readDisposition disposition
    pure
        ( case failure of
            Just record ->
                record.uploadAttempt == attempt
                    && record.code == "queue_retries_exhausted"
                    && record.detail == Nothing
                    && record.failedAt == fixedTime
                    && acked == 1
                    && retries == []
            Nothing -> False
        )

inspectionDLQFailure :: IO Bool
inspectionDLQFailure = do
    attempt <- testUploadAttemptIdentifier 2
    disposition <- newDisposition
    let dependencies =
            emptyInspectionDependencies
                { persistInspectionFailure = \_ -> throwIO (userError "D1 unavailable")
                }
    outcome <-
        try @SomeException
            ( runInspection
                "media-inspection-dlq"
                dependencies
                disposition
                (inspectionBody attempt)
                validMessageID
            )
    (acked, retries) <- readDisposition disposition
    pure (isLeft outcome && acked == 0 && retries == [])
