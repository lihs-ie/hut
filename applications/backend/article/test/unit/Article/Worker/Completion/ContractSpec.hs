{-# LANGUAGE PackageImports #-}

module Article.Worker.Completion.ContractSpec (run) where

import Article.Worker.Completion.Contract (
    interpretCompletionResponse,
    postCompletionWith,
 )
import Cloudflare.Workers.Headers (headerLookup, headersFromList)
import Cloudflare.Workers.HTTP (
    Method (POST),
    Request (..),
    Response,
    ResponseBody (ResponseBodyBytes),
    Status (Status),
    createResponse,
    requestPath,
 )
import Control.Exception (try)
import Control.Monad (forM_)
import Data.Aeson (eitherDecodeStrict')
import Data.ByteString.Lazy qualified as Lazy
import Data.Either (isLeft)
import "article" Domain.Article.Common (newArticleIdentifier)
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerated (..),
    ExcerptGeneratedMessage (..),
    newGenerationRequestIdentifier,
 )
import Presentation.Handler.Queue.ExcerptCompletion (
    CompletionDependencies (..),
    CompletionOutcome (..),
    handleCompletionMessage,
 )
import Shared.Domain.Common.Primitive (newPositiveInteger)
import Shared.Domain.Error (DomainError)
import Shared.Domain.Excerpt (newExcerpt)
import Shared.Infrastructure.Versioning (newVersion)
import Shared.UseCase.Context (newActor, newCorrelationIdentifier)
import Shared.UseCase.Event (newEventEnvelope, newEventIdentifier)
import TestSupport (check, right, timestamp)

run :: IO ()
run = do
    message <- generatedMessage
    result <- postCompletionWith pure (\stub request -> do
        check "named articles stub" (stub == "articles")
        check "completion uses POST" (request.requestMethodField == POST)
        check "completion route" $
            requestPath request == "/internal/excerpt-generation/complete"
        check "JSON content type" $
            headerLookup "content-type" request.requestHeaders == Just "application/json"
        case request.requestBodyReaderField of
            Nothing -> fail "completion request has no body reader"
            Just readBody -> do
                body <- readBody 1048576
                bytes <- either (const (fail "body read failed")) (pure . Lazy.toStrict) body
                check "complete event envelope is posted" $
                    eitherDecodeStrict' bytes == Right message
        pure (http 200)) message
    check "HTTP 200 acknowledges" (result == Right ExcerptApplied)
    check "HTTP 204 acknowledges" $
        interpretCompletionResponse (http 204) == Right ExcerptNoLongerRequired
    forM_ [201, 202, 400, 409, 500, 503] $ \status -> do
        let rejected = interpretCompletionResponse (http status)
        check ("HTTP " <> show status <> " retries") (isLeft rejected)
    failed <- try
        (handleCompletionMessage
            (CompletionDependencies (postCompletionWith pure (\_ _ -> pure (http 503))))
            message) :: IO (Either DomainError ())
    check "handler retries rejected completion" (isLeft failed)

http :: Int -> Response
http status = createResponse (Status status) (headersFromList []) (ResponseBodyBytes "")

generatedMessage :: IO ExcerptGeneratedMessage
generatedMessage = do
    identifier <- right (newEventIdentifier "completion-event")
    request <- right (newGenerationRequestIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV")
    article <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    revision <- newVersion <$> right (newPositiveInteger 3)
    excerpt <- right (newExcerpt "Generated excerpt")
    actor <- right (newActor "system")
    correlation <- right (newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAX")
    pure $ ExcerptGeneratedMessage $
        newEventEnvelope identifier (timestamp 2) actor correlation Nothing $
            ExcerptGenerated request article revision excerpt
