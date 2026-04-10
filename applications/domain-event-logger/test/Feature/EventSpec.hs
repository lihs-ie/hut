{-# LANGUAGE OverloadedStrings #-}

module Feature.EventSpec (spec) where

import API.Server (api, server)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp qualified as Warp
import Servant (serve)
import Support.Helper.API.Request
  ( mkArticleCreatedRequestBody,
    mkArticleTerminatedRequestBody,
    mkPubSubRequestBody,
  )
import Test.Hspec

withTestServer :: (Warp.Port -> IO ()) -> IO ()
withTestServer action = do
  let app = serve api server
  Warp.testWithApplication (pure app) action

postJSON :: Warp.Port -> String -> LBS.ByteString -> IO (HTTP.Response LBS.ByteString)
postJSON port path body = do
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  initialRequest <- HTTP.parseRequest ("http://localhost:" <> show port <> path)
  let httpRequest =
        initialRequest
          { HTTP.method = "POST",
            HTTP.requestBody = HTTP.RequestBodyLBS body,
            HTTP.requestHeaders = [("Content-Type", "application/json")]
          }
  HTTP.httpLbs httpRequest manager

getRequest :: Warp.Port -> String -> IO (HTTP.Response LBS.ByteString)
getRequest port path = do
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  httpRequest <- HTTP.parseRequest ("http://localhost:" <> show port <> path)
  HTTP.httpLbs httpRequest manager

spec :: Spec
spec = around withTestServer $ do
  describe "GET /health" $
    it "returns 200" $ \port -> do
      response <- getRequest port "/health"
      HTTP.responseStatus response `shouldBe` status200

  describe "POST /events/direct" $ do
    it "article.created returns 200" $ \port -> do
      response <- postJSON port "/events/direct" (mkArticleCreatedRequestBody 1)
      HTTP.responseStatus response `shouldBe` status200

    it "article.terminated returns 200" $ \port -> do
      response <- postJSON port "/events/direct" (mkArticleTerminatedRequestBody 1)
      HTTP.responseStatus response `shouldBe` status200

    it "all event types return 200 (article.created example)" $ \port -> do
      response <- postJSON port "/events/direct" (mkArticleCreatedRequestBody 2)
      HTTP.responseStatus response `shouldBe` status200

  describe "POST /events" $
    it "PubSub message returns 200" $ \port -> do
      let body = mkPubSubRequestBody (mkArticleCreatedRequestBody 1)
      response <- postJSON port "/events" body
      HTTP.responseStatus response `shouldBe` status200
