module Infrastructure.Article.Queue.ExcerptGenerationSpec (run) where

import Data.Aeson (FromJSON, Result (..), ToJSON, Value (..), eitherDecodeStrict', encode, fromJSON, toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Domain.Article.Common (newArticleIdentifier)
import Infrastructure.Article.Queue.ExcerptGeneration
import Shared.Domain.Common.Primitive (newPositiveInteger)
import Shared.Domain.Excerpt (newExcerpt)
import Shared.Infrastructure.Versioning (newVersion)
import Shared.UseCase.Context (newActor, newCorrelationIdentifier)
import Shared.UseCase.Event (newEventEnvelope, newEventIdentifier)
import TestSupport (check, right, timestamp)

run :: IO ()
run = do
    identifier <- right (newEventIdentifier "event-1")
    request <- right (newGenerationRequestIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV")
    article <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    positive <- right (newPositiveInteger 3)
    actor <- right (newActor "admin")
    correlation <- right (newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAX")
    excerpt <- right (newExcerpt "A Haskell article")
    let requested =
            ExcerptGenerationRequestedMessage
                (newEventEnvelope identifier (timestamp 1) actor correlation Nothing
                    (ExcerptGenerationRequested request article (newVersion positive)))
        generated =
            ExcerptGeneratedMessage
                (newEventEnvelope identifier (timestamp 2) actor correlation Nothing
                    (ExcerptGenerated request article (newVersion positive) excerpt))
    check "request round-trip" (decodeMessage requested == Right requested)
    check "completion round-trip" (decodeMessage generated == Right generated)
    check "zero revision rejected" (invalid generated "expectedRevision" (toJSON (0 :: Integer)))
    check "invalid excerpt rejected" (invalid generated "excerpt" (toJSON ("" :: Text)))
  where
    decodeMessage :: (FromJSON a, ToJSON a) => a -> Either String a
    decodeMessage = eitherDecodeStrict' . LazyByteString.toStrict . encode

    invalid :: ExcerptGeneratedMessage -> Text -> Value -> Bool
    invalid message field replacement =
        case toJSON message of
            Object outer -> case KeyMap.lookup "event" outer of
                Just (Object payload) ->
                    let altered = Object (KeyMap.insert (Key.fromText field) replacement payload)
                        whole = Object (KeyMap.insert "event" altered outer)
                     in case fromJSON whole :: Result ExcerptGeneratedMessage of
                            Error _ -> True
                            Success _ -> False
                _ -> False
            _ -> False
