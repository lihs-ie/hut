{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.Media.ImageAvailability (
    FetchImageStatus,
    findAvailableImages,
    findAvailableImagesWith,
) where

import Cloudflare.Workers.Binding.ServiceBinding (
    ServiceBinding,
    ServiceBindingError,
    serviceFetch,
 )
import Cloudflare.Workers.Headers (headersFromList)
import Cloudflare.Workers.HTTP (
    Method (GET),
    Request (..),
    Response (..),
    ResponseBody (..),
    Status (Status),
 )
import Cloudflare.Workers.Streaming (readableStreamToLazyByteString)
import Cloudflare.Workers.URL (parseURL)
import Control.Exception (try)
import Data.Aeson (FromJSON (..), eitherDecodeStrict', withObject, (.:))
import Data.ByteString.Lazy qualified as Lazy
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import "article" Domain.Article.Common (ImageReference, imageReferenceText)
import "shared" Shared.Domain.Error (DomainError, createServiceUnavailable)
import "shared" Shared.UseCase.Command (
    Actor,
    CorrelationIdentifier,
    actorText,
    correlationIdentifierText,
 )

type FetchImageStatus = Request -> IO (Either DomainError Response)

data ImageStatusResponse = ImageStatusResponse
    { imageIdentifier :: Text
    , state :: Text
    }

instance FromJSON ImageStatusResponse where
    parseJSON = withObject "ImageStatusResponse" $ \value ->
        ImageStatusResponse
            <$> value .: "imageIdentifier"
            <*> value .: "state"

findAvailableImages ::
    ServiceBinding ->
    Actor ->
    CorrelationIdentifier ->
    Set ImageReference ->
    IO (Either DomainError (Set ImageReference))
findAvailableImages binding = findAvailableImagesWith fetchStatus
  where
    fetchStatus request = do
        result <- try @ServiceBindingError (serviceFetch binding request)
        pure $ either
            (const (Left unavailable))
            Right
            result

findAvailableImagesWith ::
    FetchImageStatus ->
    Actor ->
    CorrelationIdentifier ->
    Set ImageReference ->
    IO (Either DomainError (Set ImageReference))
findAvailableImagesWith fetchStatus actor correlation references =
    visit Set.empty (Set.toAscList references)
  where
    visit available [] = pure (Right available)
    visit available (reference : remaining) =
        case imageRequest actor correlation reference of
            Left err -> pure (Left err)
            Right request -> do
                response <- fetchStatus request
                checked <- case response of
                    Left err -> pure (Left err)
                    Right value -> statusAvailable reference value
                case checked of
                    Left err -> pure (Left err)
                    Right True -> visit (Set.insert reference available) remaining
                    Right False -> visit available remaining

imageRequest ::
    Actor -> CorrelationIdentifier -> ImageReference -> Either DomainError Request
imageRequest actor correlation reference = do
    url <- maybe (Left unavailable) Right $
        parseURL ("https://media.internal/images/" <> imageReferenceText reference)
    pure Request
        { requestMethodField = GET
        , requestURLField = url
        , requestBodyField = Nothing
        , requestHeaders = headersFromList
            [ ("X-Hut-Actor", actorText actor)
            , ("X-Correlation-Identifier", correlationIdentifierText correlation)
            ]
        , requestBodyReaderField = Nothing
        , requestDataCenterField = Nothing
        }

statusAvailable :: ImageReference -> Response -> IO (Either DomainError Bool)
statusAvailable reference response =
    case response of
        Response (Status 404) _ _ -> pure (Right False)
        Response (Status 200) _ body -> do
            bytes <- case body of
                ResponseBodyBytes value -> pure (Right value)
                ResponseBodyLazyBytes value -> pure (Right (Lazy.toStrict value))
                ResponseBodyStream stream -> do
                    drained <- readableStreamToLazyByteString 4096 stream
                    pure (either (const (Left unavailable)) (Right . Lazy.toStrict) drained)
                _ -> pure (Left unavailable)
            pure $ do
                encoded <- bytes
                status <- either (const (Left unavailable)) Right
                    (eitherDecodeStrict' encoded :: Either String ImageStatusResponse)
                if status.imageIdentifier /= imageReferenceText reference
                    then Left unavailable
                    else case status.state of
                        "available" -> Right True
                        "awaiting_upload" -> Right False
                        "inspecting" -> Right False
                        "rejected" -> Right False
                        _ -> Left unavailable
        _ -> pure (Left unavailable)

unavailable :: DomainError
unavailable = createServiceUnavailable "Media" "image status could not be confirmed"
