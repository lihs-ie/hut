module Infrastructure.Article.Media.ImageAvailabilitySpec (run) where

import Cloudflare.Workers.Headers (headerLookup, headersFromList)
import Cloudflare.Workers.HTTP (
    Method (GET),
    Request (..),
    Response (..),
    ResponseBody (ResponseBodyBytes, ResponseBodyLazyBytes),
    Status (Status),
 )
import Cloudflare.Workers.URL (urlPath)
import Control.Monad (forM_)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.ByteString.Lazy qualified as Lazy
import Data.Set qualified as Set
import Domain.Article.Common (imageReferenceText, newImageReference)
import Infrastructure.Article.Media.ImageAvailability (findAvailableImagesWith)
import Shared.Domain.Error (DomainError (..), createServiceUnavailable)
import Shared.UseCase.Command (newActor, newCorrelationIdentifier)
import TestSupport (check, right)

run :: IO ()
run = do
    available <- right (newImageReference "01ARZ3NDEKTSV4RRFFQ69G5FAV")
    pending <- right (newImageReference "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    actor <- right (newActor "admin")
    correlation <- right (newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAX")
    called <- newIORef []
    let fetchStatus request = do
            modifyIORef' called (<> [urlPath request.requestURLField])
            check "uses Media GET" (request.requestMethodField == GET)
            check "forwards actor" $
                headerLookup "X-Hut-Actor" request.requestHeaders == Just "admin"
            check "forwards correlation" $
                headerLookup "X-Correlation-Identifier" request.requestHeaders
                    == Just "01ARZ3NDEKTSV4RRFFQ69G5FAX"
            pure $ Right $ status 200 $ if urlPath request.requestURLField
                == "/images/" <> imageReferenceText available
                then "{\"imageIdentifier\":\"01ARZ3NDEKTSV4RRFFQ69G5FAV\",\"state\":\"available\"}"
                else "{\"imageIdentifier\":\"01ARZ3NDEKTSV4RRFFQ69G5FAW\",\"state\":\"inspecting\"}"
    found <- findAvailableImagesWith fetchStatus actor correlation
        (Set.fromList [available, pending])
    check "only available references are confirmed" (found == Right (Set.singleton available))
    check "checks each distinct image once" . (== 2) . length =<< readIORef called

    empty <- findAvailableImagesWith
        (\_ -> fail "empty image set must not call Media") actor correlation Set.empty
    check "empty image references need no Media call" (empty == Right Set.empty)

    lazyResponse <- findAvailableImagesWith
        (\_ -> pure (Right (Response (Status 200) (headersFromList [])
            (ResponseBodyLazyBytes (Lazy.fromStrict
                "{\"imageIdentifier\":\"01ARZ3NDEKTSV4RRFFQ69G5FAV\",\"state\":\"available\"}")))))
        actor correlation (Set.singleton available)
    check "lazy Media response accepted" (lazyResponse == Right (Set.singleton available))

    forM_ ["awaiting_upload", "inspecting", "rejected"] $ \state -> do
        let response = status 200
                ("{\"imageIdentifier\":\"01ARZ3NDEKTSV4RRFFQ69G5FAV\",\"state\":\""
                    <> state <> "\"}")
        result <- findAvailableImagesWith (\_ -> pure (Right response))
            actor correlation (Set.singleton available)
        check "unavailable Media state excluded" (result == Right Set.empty)

    serviceFailure <- findAvailableImagesWith
        (\_ -> pure (Left (createServiceUnavailable "Media" "failed")))
        actor correlation (Set.singleton available)
    check "service failure propagated" $ case serviceFailure of
        Left ServiceUnavailable{} -> True
        _ -> False

    missing <- findAvailableImagesWith (\_ -> pure (Right (status 404 "")))
        actor correlation (Set.singleton available)
    check "missing Media image is not confirmed" (missing == Right Set.empty)

    forM_ [
        status 200 "not-json",
        status 200 "{}",
        status 200 "{\"imageIdentifier\":42,\"state\":\"available\"}",
        status 200 "{\"imageIdentifier\":\"other\",\"state\":\"available\"}",
        status 200 "{\"imageIdentifier\":\"01ARZ3NDEKTSV4RRFFQ69G5FAV\",\"state\":\"unknown\"}",
        status 204 "",
        status 503 ""
        ] $ \response -> do
            outcome <- findAvailableImagesWith (\_ -> pure (Right response))
                actor correlation (Set.singleton available)
            check "invalid Media response fails closed" $ case outcome of
                Left ServiceUnavailable{} -> True
                _ -> False
  where
    status code body = Response (Status code) (headersFromList []) (ResponseBodyBytes body)
