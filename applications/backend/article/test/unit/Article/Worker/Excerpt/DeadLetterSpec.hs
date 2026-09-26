module Article.Worker.Excerpt.DeadLetterSpec (run) where

import Article.Worker.Excerpt.DeadLetter (interpretAbandonResponse, postAbandonWith)
import Cloudflare.Workers.Headers (headersFromList)
import Cloudflare.Workers.HTTP (
    Method (POST),
    Request (..),
    Response,
    ResponseBody (ResponseBodyBytes),
    Status (Status),
    createResponse,
    requestPath,
 )
import Control.Monad (forM_)
import Data.Aeson (eitherDecode')
import Data.Either (isLeft)
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerationRequested (..),
    newGenerationRequestIdentifier,
 )
import Shared.Domain.Common.Primitive (newPositiveInteger)
import Shared.Infrastructure.Versioning (newVersion)
import TestSupport (check, identifier, right)

run :: IO ()
run = do
    article <- right identifier
    requestIdentifier <- right (newGenerationRequestIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    revision <- newVersion <$> right (newPositiveInteger 3)
    let generation = ExcerptGenerationRequested requestIdentifier article revision
    result <- postAbandonWith pure (\stub httpRequest -> do
        check "abandon uses articles DO" (stub == "articles")
        check "abandon uses POST" (httpRequest.requestMethodField == POST)
        check "abandon uses internal route" $
            requestPath httpRequest == "/internal/excerpt-generation/abandon"
        case httpRequest.requestBodyReaderField of
            Nothing -> fail "abandon request has no body"
            Just readBody -> do
                body <- readBody 1048576
                bytes <- either (const (fail "body read failed")) pure body
                check "abandon sends matching job" (eitherDecode' bytes == Right generation)
        pure (http 200)) generation
    check "HTTP 200 releases job" (result == Right ())
    check "HTTP 204 acknowledges obsolete job" $
        interpretAbandonResponse (http 204) == Right ()
    forM_ [400, 409, 500, 503] $ \status ->
        check ("HTTP " <> show status <> " retries") $
            isLeft (interpretAbandonResponse (http status))

http :: Int -> Response
http status = createResponse (Status status) (headersFromList []) (ResponseBodyBytes "")
