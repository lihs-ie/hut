module Article.Worker.Excerpt.DeadLetter (
    abandonGenerationRequest,
    postAbandonWith,
    abandonRequest,
    interpretAbandonResponse,
) where

import Cloudflare.Workers.Binding.DurableObject (
    DurableObjectNamespace,
    doFetch,
    doGetByName,
 )
import Cloudflare.Workers.Headers (headersFromList)
import Cloudflare.Workers.HTTP (
    Method (POST),
    Request (..),
    Response (..),
    Status (Status),
 )
import Cloudflare.Workers.URL (parseURL)
import Data.Aeson (encode)
import Data.Text (Text)
import Data.Text qualified as Text
import Infrastructure.Article.Queue.ExcerptGeneration (ExcerptGenerationRequested)
import "shared" Shared.Domain.Error (DomainError, createServiceUnavailable)

abandonGenerationRequest ::
    DurableObjectNamespace -> ExcerptGenerationRequested -> IO (Either DomainError ())
abandonGenerationRequest namespace request =
    postAbandonWith (doGetByName namespace) doFetch request

postAbandonWith ::
    (Text -> IO stub) ->
    (stub -> Request -> IO Response) ->
    ExcerptGenerationRequested ->
    IO (Either DomainError ())
postAbandonWith getStub fetch request =
    case abandonRequest request of
        Left err -> pure (Left err)
        Right outgoing -> do
            stub <- getStub "articles"
            response <- fetch stub outgoing
            pure (interpretAbandonResponse response)

abandonRequest :: ExcerptGenerationRequested -> Either DomainError Request
abandonRequest request = do
    url <- maybe (Left (unavailable "abandon URL is invalid")) Right $
        parseURL "https://article.internal/internal/excerpt-generation/abandon"
    pure Request
        { requestMethodField = POST
        , requestURLField = url
        , requestBodyField = Nothing
        , requestHeaders = headersFromList [("content-type", "application/json")]
        , requestBodyReaderField = Just (\_ -> pure (Right (encode request)))
        , requestDataCenterField = Nothing
        }

interpretAbandonResponse :: Response -> Either DomainError ()
interpretAbandonResponse (Response (Status 200) _ _) = Right ()
interpretAbandonResponse (Response (Status 204) _ _) = Right ()
interpretAbandonResponse (Response (Status status) _ _) =
    Left (unavailable ("abandon returned HTTP " <> Text.pack (show status)))

unavailable :: Text -> DomainError
unavailable = createServiceUnavailable "ArticleDO"
