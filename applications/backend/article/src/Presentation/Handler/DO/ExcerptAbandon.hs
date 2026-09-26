module Presentation.Handler.DO.ExcerptAbandon (
    handleExcerptAbandon,
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
import Infrastructure.Article.DurableObject.GenerationJob (GenerationFinalization (..))
import Infrastructure.Article.Queue.ExcerptGeneration (ExcerptGenerationRequested)
import "shared" Shared.Domain.Error (DomainError)

handleExcerptAbandon ::
    (ExcerptGenerationRequested -> IO (Either DomainError GenerationFinalization)) ->
    Request ->
    IO Response
handleExcerptAbandon abandon request
    | requestMethod request /= POST = pure (emptyResponse 400)
    | requestPath request /= "/internal/excerpt-generation/abandon" = pure (emptyResponse 400)
    | otherwise = case requestBodyReader request of
        Nothing -> pure (emptyResponse 400)
        Just readBody -> do
            loaded <- try @SomeException (readBody 1048576)
            case loaded of
                Left exception
                    | Just asynchronous <- fromException @SomeAsyncException exception ->
                        throwIO asynchronous
                    | otherwise -> pure (emptyResponse 500)
                Right (Left _) -> pure (emptyResponse 500)
                Right (Right body) -> case eitherDecode' body of
                    Left _ -> pure (emptyResponse 400)
                    Right generation -> do
                        result <- try @SomeException (abandon generation)
                        case result of
                            Left exception
                                | Just asynchronous <- fromException @SomeAsyncException exception ->
                                    throwIO asynchronous
                                | otherwise -> pure (emptyResponse 500)
                            Right (Left _) -> pure (emptyResponse 500)
                            Right (Right GenerationFinalized) -> pure (emptyResponse 200)
                            Right (Right GenerationFinalizationTerminalAck) ->
                                pure (emptyResponse 204)

emptyResponse :: Int -> Response
emptyResponse status =
    Response
        { responseStatus = Status status
        , responseHeaders = headersFromList [("cache-control", "no-store")]
        , responseBody = ResponseBodyBytes ""
        }
