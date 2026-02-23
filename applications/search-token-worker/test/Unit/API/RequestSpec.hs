{-# LANGUAGE OverloadedStrings #-}

module Unit.API.RequestSpec (spec) where

import API.Request (PubSubPushRequest, decodePushRequest, toDomain)
import Data.Aeson (Value, eitherDecode, encode, object, toJSON, (.=))
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Either (isLeft)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Domain.Event
  ( ArticleCreatedPayload (..),
    ArticleEditedPayload (..),
    Event (..),
    EventPayload (..),
    EventType (..),
    MemoCreatedPayload (..),
    MemoEditedPayload (..),
  )
import Test.Hspec

seedToTime :: Int -> Value
seedToTime seed = toJSON (posixSecondsToUTCTime (fromIntegral seed))

mkTimelineJSON :: Int -> Value
mkTimelineJSON seed =
  object
    [ "createdAt" .= seedToTime seed,
      "updatedAt" .= seedToTime (seed + 1)
    ]

mkArticlePayloadJSON :: Int -> Value
mkArticlePayloadJSON seed =
  object
    [ "identifier" .= ("article-" <> show seed),
      "title" .= ("title-" <> show seed),
      "content" .= ("content-" <> show seed),
      "excerpt" .= ("excerpt-" <> show seed),
      "slug" .= ("slug-" <> show seed),
      "status" .= ("published" :: String),
      "tags" .= (["tag-" <> show seed] :: [String]),
      "timeline" .= mkTimelineJSON seed
    ]

mkMemoEntryJSON :: Int -> Value
mkMemoEntryJSON seed =
  object
    [ "text" .= ("entry-" <> show seed),
      "createdAt" .= seedToTime seed
    ]

mkMemoPayloadJSON :: Int -> Value
mkMemoPayloadJSON seed =
  object
    [ "identifier" .= ("memo-" <> show seed),
      "title" .= ("title-" <> show seed),
      "slug" .= ("slug-" <> show seed),
      "entries" .= [mkMemoEntryJSON seed],
      "tags" .= (["tag-" <> show seed] :: [String]),
      "status" .= ("published" :: String),
      "timeline" .= mkTimelineJSON seed
    ]

mkEventJSON :: Int -> String -> Value -> LBS.ByteString
mkEventJSON seed eventType payload =
  encode $
    object
      [ "identifier" .= ("evt-" <> show seed),
        "occurredAt" .= seedToTime seed,
        "type" .= eventType,
        "payload" .= payload
      ]

parseAndConvert :: LBS.ByteString -> Either String Event
parseAndConvert json = do
  request <- eitherDecode json
  toDomain request

spec :: Spec
spec = do
  describe "toDomain" $ do
    context "article.created" $ do
      it "converts to ArticleCreated event" $ do
        let payload = object ["snapshot" .= mkArticlePayloadJSON 1]
            json = mkEventJSON 1 "article.created" payload
        case parseAndConvert json of
          Left err -> expectationFailure err
          Right event -> do
            event.eventType `shouldBe` ArticleCreated
            event.identifier `shouldBe` "evt-1"
            case event.payload of
              ArticleCreatedPayload' article -> do
                article.identifier `shouldBe` "article-1"
                article.title `shouldBe` "title-1"
                article.content `shouldBe` "content-1"
                article.tags `shouldBe` ["tag-1"]
              _ -> expectationFailure "Expected ArticleCreatedPayload'"

    context "article.edited" $ do
      it "converts to ArticleEdited event" $ do
        let payload =
              object
                [ "next" .= mkArticlePayloadJSON 2,
                  "before" .= mkArticlePayloadJSON 1
                ]
            json = mkEventJSON 1 "article.edited" payload
        case parseAndConvert json of
          Left err -> expectationFailure err
          Right event -> do
            event.eventType `shouldBe` ArticleEdited
            case event.payload of
              ArticleEditedPayload' edited -> do
                edited.next.identifier `shouldBe` "article-2"
                edited.before.identifier `shouldBe` "article-1"
              _ -> expectationFailure "Expected ArticleEditedPayload'"

    context "article.terminated" $ do
      it "converts to ArticleTerminated event" $ do
        let payload = object ["article" .= ("article-1" :: String)]
            json = mkEventJSON 1 "article.terminated" payload
        case parseAndConvert json of
          Left err -> expectationFailure err
          Right event -> do
            event.eventType `shouldBe` ArticleTerminated
            case event.payload of
              ArticleTerminatePayload' reference ->
                reference `shouldBe` "article-1"
              _ -> expectationFailure "Expected ArticleTerminatePayload'"

    context "memo.created" $ do
      it "converts to MemoCreated event" $ do
        let payload = object ["snapshot" .= mkMemoPayloadJSON 1]
            json = mkEventJSON 1 "memo.created" payload
        case parseAndConvert json of
          Left err -> expectationFailure err
          Right event -> do
            event.eventType `shouldBe` MemoCreated
            case event.payload of
              MemoCreatedPayload' memo -> do
                memo.identifier `shouldBe` "memo-1"
                memo.title `shouldBe` "title-1"
                memo.tags `shouldBe` ["tag-1"]
              _ -> expectationFailure "Expected MemoCreatedPayload'"

    context "memo.edited" $ do
      it "converts to MemoEdited event" $ do
        let payload =
              object
                [ "next" .= mkMemoPayloadJSON 2,
                  "before" .= mkMemoPayloadJSON 1
                ]
            json = mkEventJSON 1 "memo.edited" payload
        case parseAndConvert json of
          Left err -> expectationFailure err
          Right event -> do
            event.eventType `shouldBe` MemoEdited
            case event.payload of
              MemoEditedPayload' edited -> do
                edited.next.identifier `shouldBe` "memo-2"
                edited.before.identifier `shouldBe` "memo-1"
              _ -> expectationFailure "Expected MemoEditedPayload'"

    context "memo.terminated" $ do
      it "converts to MemoTerminated event" $ do
        let payload = object ["memo" .= ("memo-1" :: String)]
            json = mkEventJSON 1 "memo.terminated" payload
        case parseAndConvert json of
          Left err -> expectationFailure err
          Right event -> do
            event.eventType `shouldBe` MemoTerminated
            case event.payload of
              MemoTerminatePayload' reference ->
                reference `shouldBe` "memo-1"
              _ -> expectationFailure "Expected MemoTerminatePayload'"

    context "unknown event type" $ do
      it "returns Left with error message" $ do
        let json = mkEventJSON 1 "unknown.type" (object [])
        parseAndConvert json `shouldBe` Left "Unknown event type: unknown.type"

    context "invalid payload" $ do
      it "returns Left for article.created with wrong payload" $ do
        let json = mkEventJSON 1 "article.created" (object ["wrong" .= ("data" :: String)])
        parseAndConvert json `shouldSatisfy` isLeft

  describe "decodePushRequest" $ do
    context "successfully" $ do
      it "decodes valid base64 encoded EventRequest" $ do
        let eventJSON = mkEventJSON 1 "article.created" (object ["snapshot" .= mkArticlePayloadJSON 1])
            base64Data = BS.unpack (B64.encode (LBS.toStrict eventJSON))
            pubsubJSON = encode $ object ["message" .= object ["data" .= base64Data]]
        case (eitherDecode pubsubJSON :: Either String PubSubPushRequest) of
          Left err -> expectationFailure err
          Right pushRequest ->
            case decodePushRequest pushRequest of
              Left err -> expectationFailure err
              Right _ -> pure ()

    context "failure" $ do
      it "returns Left for invalid base64" $ do
        let pubsubJSON = encode $ object ["message" .= object ["data" .= ("!!!invalid-base64!!!" :: String)]]
        case (eitherDecode pubsubJSON :: Either String PubSubPushRequest) of
          Left err -> expectationFailure err
          Right pushRequest ->
            case decodePushRequest pushRequest of
              Left _ -> pure ()
              Right _ -> expectationFailure "Expected Left but got Right"

      it "returns Left for valid base64 but invalid JSON" $ do
        let base64Data = BS.unpack (B64.encode (BS.pack "not a json"))
            pubsubJSON = encode $ object ["message" .= object ["data" .= base64Data]]
        case (eitherDecode pubsubJSON :: Either String PubSubPushRequest) of
          Left err -> expectationFailure err
          Right pushRequest ->
            case decodePushRequest pushRequest of
              Left _ -> pure ()
              Right _ -> expectationFailure "Expected Left but got Right"
