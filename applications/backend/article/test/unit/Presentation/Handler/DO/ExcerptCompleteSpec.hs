module Presentation.Handler.DO.ExcerptCompleteSpec (run) where

import Cloudflare.Workers.Headers (headersFromList)
import Cloudflare.Workers.HTTP (
    Method (GET, POST),
    Request (..),
    Response (..),
    ResponseBody (ResponseBodyBytes),
    Status (Status),
 )
import Cloudflare.Workers.URL (parseURL)
import Control.Exception (throwIO)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as Lazy
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerated (..),
    ExcerptGeneratedMessage (..),
    newGenerationRequestIdentifier,
 )
import Presentation.Handler.DO.ExcerptComplete (
    CompleteOutcome (..),
    handleExcerptComplete,
 )
import Shared.Domain.Common.Primitive (newPositiveInteger)
import Shared.Domain.Error (createProcessingTargetChanged, createServiceUnavailable)
import Shared.Domain.Excerpt (newExcerpt)
import Shared.Infrastructure.Versioning (newVersion)
import Shared.UseCase.Command (newActor, newCorrelationIdentifier)
import Shared.UseCase.Event (newEventEnvelope, newEventIdentifier)
import TestSupport (check, identifier, right, timestamp)

path :: Text
path = "https://article.internal/internal/excerpt-generation/complete"

requestAt :: Method -> Maybe Lazy.ByteString -> IO Request
requestAt method body = do
    url <- maybe (fail "invalid URL") pure (parseURL path)
    pure Request
        { requestMethodField = method
        , requestURLField = url
        , requestBodyField = Nothing
        , requestHeaders = headersFromList []
        , requestBodyReaderField = fmap (\bytes _ -> pure (Right bytes)) body
        , requestDataCenterField = Nothing
        }

message :: IO ExcerptGeneratedMessage
message = do
    event <- right (newEventIdentifier "completion-event")
    request <- right (newGenerationRequestIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    article <- right identifier
    revision <- newVersion <$> right (newPositiveInteger 3)
    excerpt <- right (newExcerpt "Generated excerpt")
    actor <- right (newActor "system")
    correlation <- right (newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAX")
    pure $ ExcerptGeneratedMessage $
        newEventEnvelope event (timestamp 2) actor correlation Nothing
            (ExcerptGenerated request article revision excerpt)

statusIs :: Int -> Response -> Bool
statusIs status response = response.responseStatus == Status status

run :: IO ()
run = do
    handlesCompletedAndObsolete
    rejectsMalformedRequests
    retriesTransientFailures

handlesCompletedAndObsolete :: IO ()
handlesCompletedAndObsolete = do
    event <- message
    called <- newIORef False
    request <- requestAt POST (Just (encode event))
    completed <- handleExcerptComplete
        (\actual -> writeIORef called (actual == event) >> pure (Right ExcerptCompleted))
        request
    check "completion returns 200" (statusIs 200 completed)
    check "completion decodes event" =<< readIORef called
    check "completion has empty response body" $ case completed.responseBody of
        ResponseBodyBytes bytes -> bytes == ""
        _ -> False
    obsolete <- handleExcerptComplete (\_ -> pure (Right CompletionObsolete)) request
    check "obsolete completion returns 204" (statusIs 204 obsolete)
    changed <- handleExcerptComplete
        (\_ -> pure (Left (createProcessingTargetChanged "Article" "changed")))
        request
    check "changed revision is acknowledged" (statusIs 204 changed)

rejectsMalformedRequests :: IO ()
rejectsMalformedRequests = do
    called <- newIORef False
    let apply _ = writeIORef called True >> pure (Right ExcerptCompleted)
    missing <- requestAt POST Nothing >>= handleExcerptComplete apply
    invalid <- requestAt POST (Just "not-json") >>= handleExcerptComplete apply
    wrongMethod <- requestAt GET (Just "not-json") >>= handleExcerptComplete apply
    check "missing body returns 400" (statusIs 400 missing)
    check "invalid message returns 400" (statusIs 400 invalid)
    check "wrong method returns 400" (statusIs 400 wrongMethod)
    check "invalid requests skip apply" . not =<< readIORef called

retriesTransientFailures :: IO ()
retriesTransientFailures = do
    event <- message
    request <- requestAt POST (Just (encode event))
    unavailable <- handleExcerptComplete
        (\_ -> pure (Left (createServiceUnavailable "Article" "unavailable")))
        request
    check "service failure returns 500" (statusIs 500 unavailable)
    exception <- handleExcerptComplete (\_ -> throwIO (userError "failed")) request
    check "synchronous exception returns 500" (statusIs 500 exception)
    let readerFailure = request
            { requestBodyReaderField = Just (\_ -> fail "read failed")
            }
    unreadable <- handleExcerptComplete (\_ -> pure (Right ExcerptCompleted)) readerFailure
    check "request stream failure returns 500" (statusIs 500 unreadable)
