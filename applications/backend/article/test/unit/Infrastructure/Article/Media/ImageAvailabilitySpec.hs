module Infrastructure.Article.Media.ImageAvailabilitySpec (run) where

import Cloudflare.Workers.Headers (headerLookup, headersFromList)
import Cloudflare.Workers.HTTP (
    Method (POST),
    Request (..),
    Response (..),
    ResponseBody (ResponseBodyBytes, ResponseBodyLazyBytes),
    Status (Status),
 )
import Cloudflare.Workers.URL (urlPath)
import Control.Monad (forM_)
import Data.Aeson (Value, decode, object, (.=))
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.ByteString.Lazy qualified as Lazy
import Data.Set qualified as Set
import Domain.Article.Common (newImageReference)
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
    called <- newIORef (0 :: Int)
    let fetchAvailability request = do
            modifyIORef' called (+ 1)
            check "uses Media batch POST" (request.requestMethodField == POST)
            check "uses availability route" $
                urlPath request.requestURLField == "/images/availability"
            check "uses JSON content type" $
                headerLookup "Content-Type" request.requestHeaders
                    == Just "application/json"
            check "forwards actor" $
                headerLookup "X-Hut-Actor" request.requestHeaders == Just "admin"
            check "forwards correlation" $
                headerLookup "X-Correlation-Identifier" request.requestHeaders
                    == Just "01ARZ3NDEKTSV4RRFFQ69G5FAX"
            body <- case request.requestBodyReaderField of
                Just readBody -> readBody 4096
                Nothing -> fail "missing request body"
            bytes <- either (const (fail "unreadable request body")) pure body
            check "sends both image references" $
                (decode bytes :: Maybe Value)
                    == Just
                        ( object
                            [ "images" .=
                                [ "01ARZ3NDEKTSV4RRFFQ69G5FAV" :: String
                                , "01ARZ3NDEKTSV4RRFFQ69G5FAW"
                                ]
                            ]
                        )
            pure (Right (status 200
                "{\"available\":[\"01ARZ3NDEKTSV4RRFFQ69G5FAV\"]}"))
    found <- findAvailableImagesWith fetchAvailability actor correlation
        (Set.fromList [available, pending])
    check "only available references are confirmed" (found == Right (Set.singleton available))
    check "one Media request checks all images" . (== 1) =<< readIORef called

    empty <- findAvailableImagesWith
        (\_ -> fail "empty image set must not call Media") actor correlation Set.empty
    check "empty image references need no Media call" (empty == Right Set.empty)

    lazyResponse <- findAvailableImagesWith
        (\_ -> pure (Right (Response (Status 200) (headersFromList [])
            (ResponseBodyLazyBytes (Lazy.fromStrict
                "{\"available\":[\"01ARZ3NDEKTSV4RRFFQ69G5FAV\"]}")))))
        actor correlation (Set.singleton available)
    check "lazy Media response accepted" (lazyResponse == Right (Set.singleton available))

    unavailable <- findAvailableImagesWith
        (\_ -> pure (Right (status 200 "{\"available\":[]}")))
        actor correlation (Set.singleton pending)
    check "unavailable image is excluded" (unavailable == Right Set.empty)

    serviceFailure <- findAvailableImagesWith
        (\_ -> pure (Left (createServiceUnavailable "Media" "failed")))
        actor correlation (Set.singleton available)
    check "service failure propagated" $ case serviceFailure of
        Left ServiceUnavailable{} -> True
        _ -> False

    forM_
        [ status 200 "not-json"
        , status 200 "{}"
        , status 200 "{\"available\":42}"
        , status 200 "{\"available\":[\"invalid\"]}"
        , status 200 "{\"available\":[\"01ARZ3NDEKTSV4RRFFQ69G5FAW\"]}"
        , status 204 ""
        , status 404 ""
        , status 503 ""
        ] $ \response -> do
            outcome <- findAvailableImagesWith (\_ -> pure (Right response))
                actor correlation (Set.singleton available)
            check "invalid Media response fails closed" $ case outcome of
                Left ServiceUnavailable{} -> True
                _ -> False
  where
    status code body = Response (Status code) (headersFromList []) (ResponseBodyBytes body)
