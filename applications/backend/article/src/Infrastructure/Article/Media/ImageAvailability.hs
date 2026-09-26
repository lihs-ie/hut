{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.Media.ImageAvailability (
    FetchImageAvailability,
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
    Method (POST),
    Request (..),
    Response (..),
    ResponseBody (..),
    Status (Status),
 )
import Cloudflare.Workers.Streaming (readableStreamToLazyByteString)
import Cloudflare.Workers.URL (parseURL)
import Control.Exception (try)
import Data.Aeson (FromJSON (..), eitherDecodeStrict', encode, object, withObject, (.:), (.=))
import Data.ByteString.Lazy qualified as Lazy
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import "article" Domain.Article.Common (
    ImageReference,
    imageReferenceText,
    newImageReference,
 )
import "shared" Shared.Domain.Error (DomainError, createServiceUnavailable)
import "shared" Shared.UseCase.Command (
    Actor,
    CorrelationIdentifier,
    actorText,
    correlationIdentifierText,
 )

type FetchImageAvailability = Request -> IO (Either DomainError Response)

newtype AvailabilityResponse = AvailabilityResponse [Text]

instance FromJSON AvailabilityResponse where
    parseJSON = withObject "AvailabilityResponse" $ \value ->
        AvailabilityResponse <$> value .: "available"

findAvailableImages ::
    ServiceBinding ->
    Actor ->
    CorrelationIdentifier ->
    Set ImageReference ->
    IO (Either DomainError (Set ImageReference))
findAvailableImages binding = findAvailableImagesWith fetchAvailability
  where
    fetchAvailability request = do
        result <- try @ServiceBindingError (serviceFetch binding request)
        pure $ either (const (Left unavailable)) Right result

findAvailableImagesWith ::
    FetchImageAvailability ->
    Actor ->
    CorrelationIdentifier ->
    Set ImageReference ->
    IO (Either DomainError (Set ImageReference))
findAvailableImagesWith _ _ _ references | Set.null references = pure (Right Set.empty)
findAvailableImagesWith fetchAvailability actor correlation references =
    case availabilityRequest actor correlation references of
        Left err -> pure (Left err)
        Right request -> do
            response <- fetchAvailability request
            case response of
                Left err -> pure (Left err)
                Right value -> parseAvailability references value

availabilityRequest ::
    Actor -> CorrelationIdentifier -> Set ImageReference -> Either DomainError Request
availabilityRequest actor correlation references = do
    url <- maybe (Left unavailable) Right $
        parseURL "https://media.internal/images/availability"
    let identifiers = map imageReferenceText (Set.toAscList references)
    pure Request
        { requestMethodField = POST
        , requestURLField = url
        , requestBodyField = Nothing
        , requestHeaders = headersFromList
            [ ("Content-Type", "application/json")
            , ("X-Hut-Actor", actorText actor)
            , ("X-Correlation-Identifier", correlationIdentifierText correlation)
            ]
        , requestBodyReaderField = Just $ \_ ->
            pure (Right (encode (object ["images" .= identifiers])))
        , requestDataCenterField = Nothing
        }

parseAvailability ::
    Set ImageReference -> Response -> IO (Either DomainError (Set ImageReference))
parseAvailability requested response =
    case response of
        Response (Status 200) _ body -> do
            bytes <- case body of
                ResponseBodyBytes value -> pure (Right value)
                ResponseBodyLazyBytes value -> pure (Right (Lazy.toStrict value))
                ResponseBodyStream stream -> do
                    drained <- readableStreamToLazyByteString (2 * 1024 * 1024) stream
                    pure (either (const (Left unavailable)) (Right . Lazy.toStrict) drained)
                _ -> pure (Left unavailable)
            pure $ do
                encoded <- bytes
                AvailabilityResponse available <- either
                    (const (Left unavailable))
                    Right
                    (eitherDecodeStrict' encoded :: Either String AvailabilityResponse)
                identifiers <- either (const (Left unavailable)) Right $
                    traverse newImageReference available
                let confirmed = Set.fromList identifiers
                if confirmed `Set.isSubsetOf` requested
                    then Right confirmed
                    else Left unavailable
        _ -> pure (Left unavailable)

unavailable :: DomainError
unavailable = createServiceUnavailable "Media" "image availability could not be confirmed"
