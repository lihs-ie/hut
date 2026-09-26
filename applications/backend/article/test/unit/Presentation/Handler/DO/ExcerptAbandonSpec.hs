module Presentation.Handler.DO.ExcerptAbandonSpec (run) where

import Cloudflare.Workers.Headers (headersFromList)
import Cloudflare.Workers.HTTP (
    Method (GET, POST),
    Request (..),
    Response (..),
    Status (Status),
 )
import Cloudflare.Workers.URL (parseURL)
import Control.Exception (AsyncException (ThreadKilled), SomeException, throwIO, try)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as Lazy
import Data.IORef (newIORef, readIORef, writeIORef)
import Infrastructure.Article.DurableObject.GenerationJob (GenerationFinalization (..))
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerationRequested (..),
    newGenerationRequestIdentifier,
 )
import Presentation.Handler.DO.ExcerptAbandon (handleExcerptAbandon)
import Shared.Domain.Common.Primitive (newPositiveInteger)
import Shared.Domain.Error (createServiceUnavailable)
import Shared.Infrastructure.Versioning (newVersion)
import TestSupport (check, identifier, right)

run :: IO ()
run = do
    article <- right identifier
    requestIdentifier <- right (newGenerationRequestIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    revision <- newVersion <$> right (newPositiveInteger 3)
    let generation = ExcerptGenerationRequested requestIdentifier article revision
    called <- newIORef False
    request <- requestAt POST (Just (encode generation))
    completed <- handleExcerptAbandon
        (\actual -> writeIORef called (actual == generation) >> pure (Right GenerationFinalized))
        request
    check "abandon returns 200" (statusIs 200 completed)
    check "abandon decodes request" =<< readIORef called
    obsolete <- handleExcerptAbandon
        (\_ -> pure (Right GenerationFinalizationTerminalAck)) request
    check "obsolete abandon returns 204" (statusIs 204 obsolete)
    missing <- requestAt POST Nothing >>= handleExcerptAbandon
        (\_ -> pure (Right GenerationFinalized))
    invalid <- requestAt POST (Just "not-json") >>= handleExcerptAbandon
        (\_ -> pure (Right GenerationFinalized))
    wrongMethod <- requestAt GET (Just (encode generation)) >>= handleExcerptAbandon
        (\_ -> pure (Right GenerationFinalized))
    check "missing body rejected" (statusIs 400 missing)
    check "invalid body rejected" (statusIs 400 invalid)
    check "wrong method rejected" (statusIs 400 wrongMethod)
    wrongURL <- maybe (fail "invalid URL") pure $
        parseURL "https://article.internal/internal/excerpt-generation/claim"
    wrongPath <- handleExcerptAbandon
        (\_ -> pure (Right GenerationFinalized))
        request{requestURLField = wrongURL}
    check "wrong path rejected" (statusIs 400 wrongPath)
    unreadable <- handleExcerptAbandon
        (\_ -> pure (Right GenerationFinalized))
        request{requestBodyReaderField = Just (\_ -> pure (Left undefined))}
    check "unreadable body retries" (statusIs 500 unreadable)
    readFailure <- handleExcerptAbandon
        (\_ -> pure (Right GenerationFinalized))
        request{requestBodyReaderField = Just (\_ -> throwIO (userError "read failed"))}
    check "body reader exception retries" (statusIs 500 readFailure)
    asyncRead <- try (handleExcerptAbandon
        (\_ -> pure (Right GenerationFinalized))
        request{requestBodyReaderField = Just (\_ -> throwIO ThreadKilled)})
        :: IO (Either SomeException Response)
    check "async body reader exception propagates" (either (const True) (const False) asyncRead)
    unavailable <- handleExcerptAbandon
        (\_ -> pure (Left (createServiceUnavailable "ArticleDO" "unavailable"))) request
    check "unavailable DO retries" (statusIs 500 unavailable)
    exception <- handleExcerptAbandon (\_ -> throwIO (userError "failed")) request
    check "synchronous exception retries" (statusIs 500 exception)
    asyncFailure <- try (handleExcerptAbandon (\_ -> throwIO ThreadKilled) request)
        :: IO (Either SomeException Response)
    check "async exception propagates" (either (const True) (const False) asyncFailure)

requestAt :: Method -> Maybe Lazy.ByteString -> IO Request
requestAt method body = do
    url <- maybe (fail "invalid URL") pure $
        parseURL "https://article.internal/internal/excerpt-generation/abandon"
    pure Request
        { requestMethodField = method
        , requestURLField = url
        , requestBodyField = Nothing
        , requestHeaders = headersFromList []
        , requestBodyReaderField = fmap (\bytes _ -> pure (Right bytes)) body
        , requestDataCenterField = Nothing
        }

statusIs :: Int -> Response -> Bool
statusIs status response = response.responseStatus == Status status
