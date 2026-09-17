module Media.Presentation.Handler.ReferenceProjectionQueueSpec (run) where

import Cloudflare.Workers.Entrypoint.Queue (
    QueueRetryOptions (QueueRetryOptions),
 )
import Control.Exception (throwIO)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Media.Presentation.Handler.QueueTestSupport (
    executionContext,
    fixedTime,
    named,
    newDisposition,
    projectionBody,
    projectionBodyWith,
    queueBatch,
    queueMessage,
    queueMessageForBody,
    readDisposition,
    showText,
    validMessageID,
 )
import Media.Presentation.Handler.ReferenceProjectionQueue (
    ReferenceProjectionHandlerDependencies (ReferenceProjectionHandlerDependencies),
    mediaReferenceProjectionQueueHandler,
 )
import "media" Media.Domain.ImageUsage (
    SourceKind (..),
    foldImageUsageProjection,
    imageReferenceText,
    sourceIdentifierText,
    sourcePositionText,
 )
import "media" Media.UseCase.ProcessImageInspection (

 )
import "media" Media.UseCase.ProjectImageUsage (
    ImageUsageProjectionStore (ImageUsageProjectionStore),
    ProjectionApplyResult (..),
 )

run :: IO Bool
run =
    and
        <$> sequence
            [ named "projection source kinds" projectionSourceKinds
            , named "projection store outcomes" projectionStoreOutcomes
            , named "invalid projection messages acknowledge" projectionInvalidInputs
            , named "projection store failure retries" projectionStoreFailure
            ]

projectionSourceKinds :: IO Bool
projectionSourceKinds = do
    captured <- newIORef []
    dispositions <- traverse (const newDisposition) sourceCases
    let store = ImageUsageProjectionStore $ \event projection -> do
            let flattened =
                    foldImageUsageProjection
                        ( \kind source position references occurredAt ->
                            ( event
                            , kind
                            , sourceIdentifierText source
                            , sourcePositionText position
                            , fmap imageReferenceText references
                            , occurredAt
                            )
                        )
                        projection
            modifyIORef' captured (<> [flattened])
            pure ProjectionApplied
        messages =
            zipWith
                ( \(kind, _) disposition ->
                    queueMessage
                        disposition
                        validMessageID
                        (projectionBody kind ["image-a", "image-a"])
                )
                sourceCases
                dispositions
    mediaReferenceProjectionQueueHandler
        (queueBatch "media-reference" messages)
        (ReferenceProjectionHandlerDependencies store)
        executionContext
    actual <- readIORef captured
    dispositionValues <- traverse readDisposition dispositions
    pure
        ( actual
            == [ ("event-article", ArticleSource, "source-1", "position-1", ["image-a"], fixedTime)
               , ("event-memo", MemoSource, "source-1", "position-1", ["image-a"], fixedTime)
               , ("event-series", SeriesSource, "source-1", "position-1", ["image-a"], fixedTime)
               ]
            && all (== (1, [])) dispositionValues
        )
  where
    sourceCases =
        [ ("article", ArticleSource)
        , ("memo", MemoSource)
        , ("series", SeriesSource)
        ]

projectionStoreOutcomes :: IO Bool
projectionStoreOutcomes = do
    dispositions <- traverse (const newDisposition) outcomes
    storeResults <- newIORef outcomes
    let messages =
            zipWith
                ( \index disposition ->
                    queueMessage
                        disposition
                        validMessageID
                        (projectionBody "article" ["image-" <> showText index])
                )
                [1 :: Int ..]
                dispositions
        store = ImageUsageProjectionStore $ \_ _ -> do
            remaining <- readIORef storeResults
            case remaining of
                result : rest -> writeIORef storeResults rest >> pure result
                [] -> error "unexpected projection call"
    mediaReferenceProjectionQueueHandler
        (queueBatch "media-reference" messages)
        (ReferenceProjectionHandlerDependencies store)
        executionContext
    dispositionValues <- traverse readDisposition dispositions
    pure (all (== (1, [])) dispositionValues)
  where
    outcomes = [ProjectionApplied, ProjectionDuplicate, ProjectionOutOfOrder]

projectionInvalidInputs :: IO Bool
projectionInvalidInputs = do
    let bodies =
            [ "not-json"
            , projectionBody "unknown" ["image-a"]
            , projectionBodyWith "article" "" "position-1" ["image-a"]
            , projectionBodyWith "article" "source-1" " " ["image-a"]
            , projectionBody "article" [""]
            ]
    dispositions <- traverse (const newDisposition) bodies
    let messages = zipWith (queueMessageForBody validMessageID) dispositions bodies
        store = ImageUsageProjectionStore (\_ _ -> error "invalid DTO must not be stored")
    mediaReferenceProjectionQueueHandler
        (queueBatch "media-reference" messages)
        (ReferenceProjectionHandlerDependencies store)
        executionContext
    dispositionValues <- traverse readDisposition dispositions
    pure (all (== (0, [QueueRetryOptions Nothing])) dispositionValues)

projectionStoreFailure :: IO Bool
projectionStoreFailure = do
    disposition <- newDisposition
    let store = ImageUsageProjectionStore $ \_ _ -> throwIO (userError "D1 unavailable")
    mediaReferenceProjectionQueueHandler
        ( queueBatch
            "media-reference"
            [queueMessage disposition validMessageID (projectionBody "article" ["image-a"])]
        )
        (ReferenceProjectionHandlerDependencies store)
        executionContext
    (acked, retries) <- readDisposition disposition
    pure (acked == 0 && retries == [QueueRetryOptions Nothing])
