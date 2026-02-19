{-# LANGUAGE OverloadedStrings #-}

module Unit.API.ServerSpec (spec) where

import API.Server (api, server)
import Aspects.Log (LogEntry)
import Control.Monad.Writer (WriterT)
import Data.Aeson (encode, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Domain.SearchToken (Persist, SearchTokenError (..), TerminateByReference)
import Network.HTTP.Types (methodPost)
import Network.Wai (Application)
import Network.Wai.Test (SResponse)
import Servant (serve)
import Support.Helper.API.Request
  ( mkArticleCreatedRequestBody,
    mkArticleTerminatedRequestBody,
    mkPubSubRequestBody,
  )
import Test.Hspec
import Test.Hspec.Wai

createMockApplication :: Either SearchTokenError () -> Either SearchTokenError () -> IO Application
createMockApplication persistResult terminateResult =
  pure $ serve api (server persist terminate)
  where
    persist :: Persist (WriterT [LogEntry] IO)
    persist _tokens = pure persistResult
    terminate :: TerminateByReference (WriterT [LogEntry] IO)
    terminate _reference = pure terminateResult

jsonPost :: ByteString -> LBS.ByteString -> WaiSession st SResponse
jsonPost path = request methodPost path [("Content-Type", "application/json")]

spec :: Spec
spec = do
  describe "GET /health" $
    with (createMockApplication (Right ()) (Right ())) $
      it "returns 200" $
        get "/health" `shouldRespondWith` 200

  describe "POST /events/direct" $
    context "successfully" $ do
      with (createMockApplication (Right ()) (Right ())) $ do
        it "article.created returns 200" $
          jsonPost "/events/direct" (mkArticleCreatedRequestBody 1)
            `shouldRespondWith` 200

        it "article.created returns 200" $
          jsonPost "/events/direct" (mkArticleTerminatedRequestBody 1)
            `shouldRespondWith` 200

  context "unsuccessfully" $ do
    context "invalid request" $
      with (createMockApplication (Right ()) (Right ())) $ do
        it "unknown event type returns 400" $ do
          let body =
                encode $
                  object
                    [ "identifier" .= ("evt-1" :: String),
                      "occurredAt" .= ("1970-01-01T00:00:01Z" :: String),
                      "type" .= ("unknown.type" :: String),
                      "payload" .= object []
                    ]
          jsonPost "/events/direct" body
            `shouldRespondWith` 400

        it "invalid JSON returns 400" $
          jsonPost "/events/direct" "{invalid" `shouldRespondWith` 400

    context "handler failure" $
      with (createMockApplication (Left Unexpected) (Right ())) $ do
        it "persist failure returns 500" $
          jsonPost "/events/direct" (mkArticleCreatedRequestBody 1) `shouldRespondWith` 500

  describe "POST /events" $ do
    context "successfully" $
      with (createMockApplication (Right ()) (Right ())) $
        it "valid PubSub message returns 200" $
          jsonPost "/events" (mkPubSubRequestBody (mkArticleCreatedRequestBody 1))
            `shouldRespondWith` 200

    context "unsuccessfully" $
      with (createMockApplication (Right ()) (Right ())) $ do
        it "invalid base64 returns 400" $ do
          let body = encode $ object ["message" .= object ["data" .= ("!!!invalid!!!" :: String)]]
          jsonPost "/events" body `shouldRespondWith` 400

        it "invalid JSON body returns 400" $
          jsonPost "/events" "{bad"
            `shouldRespondWith` 400
