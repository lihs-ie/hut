module Presentation.Handler.DO.ExcerptComplete (
    CompleteOutcome (..),
    handleExcerptComplete,
) where

import Cloudflare.Workers.Headers (headersFromList)
import Cloudflare.Workers.HTTP (
    Method (POST),
    Request,
    Response (..),
    ResponseBody (ResponseBodyBytes),
    Status (Status),
    requestBodyReader,
    requestMethod,
    requestPath,
 )
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Aeson (eitherDecode')
import Infrastructure.Article.Queue.ExcerptGeneration (ExcerptGeneratedMessage)
import "shared" Shared.Domain.Error (DomainError (..))

data CompleteOutcome = ExcerptCompleted | CompletionObsolete
    deriving stock (Show, Eq)

handleExcerptComplete ::
    (ExcerptGeneratedMessage -> IO (Either DomainError CompleteOutcome)) ->
    Request ->
    IO Response
handleExcerptComplete apply request
    | requestMethod request /= POST = pure badRequest
    | requestPath request /= "/internal/excerpt-generation/complete" = pure badRequest
    | otherwise = case requestBodyReader request of
        Nothing -> pure badRequest
        Just readBody -> do
            loaded <- try @SomeException (readBody 1048576)
            case loaded of
                Left exception
                    | Just asynchronous <- fromException @SomeAsyncException exception ->
                        throwIO asynchronous
                    | otherwise -> pure storageFailure
                Right (Left _) -> pure storageFailure
                Right (Right body) -> case eitherDecode' body of
                    Left _ -> pure badRequest
                    Right message -> do
                        applied <- try @SomeException (apply message)
                        case applied of
                            Left exception
                                | Just asynchronous <- fromException @SomeAsyncException exception ->
                                    throwIO asynchronous
                                | otherwise -> pure storageFailure
                            Right (Right ExcerptCompleted) -> pure (emptyResponse 200)
                            Right (Right CompletionObsolete) -> pure (emptyResponse 204)
                            Right (Left (ProcessingTargetChanged _)) -> pure (emptyResponse 204)
                            Right (Left _) -> pure storageFailure

badRequest :: Response
badRequest = emptyResponse 400

storageFailure :: Response
storageFailure = emptyResponse 500

emptyResponse :: Int -> Response
emptyResponse status =
    Response
        { responseStatus = Status status
        , responseHeaders = headersFromList [("cache-control", "no-store")]
        , responseBody = ResponseBodyBytes ""
        }
