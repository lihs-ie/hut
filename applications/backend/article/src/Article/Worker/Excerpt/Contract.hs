module Article.Worker.Excerpt.Contract (
    claimArticleContent,
    claimRequest,
    decodeClaimedContent,
    interpretClaimResponse,
) where

import Cloudflare.Workers.Binding.DurableObject (
    DurableObjectNamespace,
    doFetch,
    doGetByName,
 )
import Cloudflare.Workers.Headers (headersFromList)
import Cloudflare.Workers.HTTP (
    Method (GET),
    Request (..),
    Response (..),
    ResponseBody (ResponseBodyBytes),
    Status (Status),
 )
import Cloudflare.Workers.URL (parseURL, percentEncodeQueryComponent)
import Data.Aeson (FromJSON (..), eitherDecodeStrict', withObject, (.:))
import Data.ByteString (ByteString)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import "article" Domain.Article.Common (
    DraftInput (..),
    ProofreadedContent,
    articleIdentifierText,
    confirmAvailableImageReferences,
    newDraftContent,
    newImageReference,
    proofreadContent,
 )
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerationRequested (..),
    generationRequestIdentifierText,
 )
import "shared" Shared.Domain.Error (DomainError, createServiceUnavailable)
import "shared" Shared.Infrastructure.Versioning (versionInteger)

data ClaimedContent = ClaimedContent
    { article :: Text
    , request :: Text
    , expectedRevision :: Integer
    , title :: Text
    , body :: Text
    , slug :: Text
    , tags :: [Text]
    , images :: [Text]
    }

instance FromJSON ClaimedContent where
    parseJSON = withObject "ClaimedContent" $ \value -> do
        content <- value .: "content"
        ClaimedContent
            <$> value .: "article"
            <*> value .: "request"
            <*> value .: "expectedRevision"
            <*> content .: "title"
            <*> content .: "body"
            <*> content .: "slug"
            <*> content .: "tags"
            <*> content .: "images"

-- The Article DO must implement this internal route. A 200 response is only
-- valid after it checks the active request, article revision and Proofreaded
-- state in one consistent storage transaction. A 204 means the request is obsolete.
-- Any other status or malformed 200 response is retried, not acknowledged.
claimArticleContent ::
    DurableObjectNamespace ->
    ExcerptGenerationRequested ->
    IO (Either DomainError (Maybe ProofreadedContent))
claimArticleContent namespace request = do
    case claimRequest request of
        Left err -> pure (Left err)
        Right outgoing -> do
            stub <- doGetByName namespace "articles"
            response <- doFetch stub outgoing
            pure (interpretClaimResponse request response)

interpretClaimResponse ::
    ExcerptGenerationRequested -> Response -> Either DomainError (Maybe ProofreadedContent)
interpretClaimResponse request response = case response of
    Response (Status 204) _ _ -> Right Nothing
    Response (Status 200) _ (ResponseBodyBytes bytes) ->
        Just <$> decodeClaimedContent request bytes
    Response (Status status) _ _ ->
        Left (unavailable ("claim returned HTTP " <> Text.pack (show status)))

claimRequest :: ExcerptGenerationRequested -> Either DomainError Request
claimRequest request = do
    url <- maybe (Left (unavailable "claim URL is invalid")) Right (parseURL path)
    pure Request
        { requestMethodField = GET
        , requestURLField = url
        , requestBodyField = Nothing
        , requestHeaders = headersFromList [("accept", "application/json")]
        , requestBodyReaderField = Nothing
        , requestDataCenterField = Nothing
        }
  where
    path =
        "https://article.internal/internal/excerpt-generation/claim"
            <> "?article=" <> encoded (articleIdentifierText request.article)
            <> "&request=" <> encoded (generationRequestIdentifierText request.identifier)
            <> "&expectedRevision=" <> Text.pack (show (versionInteger request.expectedRevision))
    encoded = percentEncodeQueryComponent

decodeClaimedContent ::
    ExcerptGenerationRequested -> ByteString -> Either DomainError ProofreadedContent
decodeClaimedContent request bytes = do
    content <- either (Left . unavailable . Text.pack) Right (eitherDecodeStrict' bytes)
    validateContent request content

validateContent ::
    ExcerptGenerationRequested -> ClaimedContent -> Either DomainError ProofreadedContent
validateContent request content
    | content.article /= articleIdentifierText request.article = Left mismatch
    | content.request /= generationRequestIdentifierText request.identifier = Left mismatch
    | content.expectedRevision /= versionInteger request.expectedRevision = Left mismatch
    | otherwise = do
        references <- either (const (Left invalidContent)) Right
            (traverse newImageReference content.images)
        let images = Set.fromList references
        if Set.size images /= length references
            then Left invalidContent
            else do
                draft <- either (const (Left invalidContent)) Right $
                    newDraftContent (const (Right images))
                        DraftInput
                            { title = content.title
                            , body = content.body
                            , slug = Just content.slug
                            , tags = content.tags
                            }
                available <- either (const (Left invalidContent)) Right $
                    confirmAvailableImageReferences images images
                either (const (Left invalidContent)) Right (proofreadContent available draft)
  where
    mismatch = unavailable "claim response does not match the queued request"
    invalidContent = unavailable "claim response contains invalid article content"

unavailable :: Text -> DomainError
unavailable = createServiceUnavailable "ArticleDO"
