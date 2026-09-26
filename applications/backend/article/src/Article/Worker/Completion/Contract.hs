module Article.Worker.Completion.Contract (
    applyGeneratedExcerpt,
    postCompletionWith,
    completionRequest,
    interpretCompletionResponse,
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
import Infrastructure.Article.Queue.ExcerptGeneration (ExcerptGeneratedMessage)
import Presentation.Handler.Queue.ExcerptCompletion (CompletionOutcome (..))
import "shared" Shared.Domain.Error (DomainError, createServiceUnavailable)

applyGeneratedExcerpt ::
    DurableObjectNamespace ->
    ExcerptGeneratedMessage ->
    IO (Either DomainError CompletionOutcome)
applyGeneratedExcerpt namespace =
    postCompletionWith (doGetByName namespace) doFetch

postCompletionWith ::
    (Text -> IO stub) ->
    (stub -> Request -> IO Response) ->
    ExcerptGeneratedMessage ->
    IO (Either DomainError CompletionOutcome)
postCompletionWith getStub fetch message =
    case completionRequest message of
        Left err -> pure (Left err)
        Right request -> do
            stub <- getStub "articles"
            response <- fetch stub request
            pure (interpretCompletionResponse response)

completionRequest :: ExcerptGeneratedMessage -> Either DomainError Request
completionRequest message = do
    url <- maybe (Left (unavailable "completion URL is invalid")) Right $
        parseURL "https://article.internal/internal/excerpt-generation/complete"
    pure Request
        { requestMethodField = POST
        , requestURLField = url
        , requestBodyField = Nothing
        , requestHeaders = headersFromList [("content-type", "application/json")]
        , requestBodyReaderField = Just (\_ -> pure (Right (encode message)))
        , requestDataCenterField = Nothing
        }

interpretCompletionResponse :: Response -> Either DomainError CompletionOutcome
interpretCompletionResponse (Response (Status 200) _ _) = Right ExcerptApplied
interpretCompletionResponse (Response (Status 204) _ _) = Right ExcerptNoLongerRequired
interpretCompletionResponse (Response (Status status) _ _) =
    Left (unavailable ("completion returned HTTP " <> Text.pack (show status)))

unavailable :: Text -> DomainError
unavailable = createServiceUnavailable "ArticleDO"
