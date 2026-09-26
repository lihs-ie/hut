{-# LANGUAGE PackageImports #-}

module Presentation.Server.APISpec (run) where

import Cloudflare.Workers.Headers (headerLookup, headersFromList)
import Cloudflare.Workers.HostTestKit (phantomJSVal)
import Cloudflare.Workers.HTTP (
    Method (GET, POST),
    Request (..),
    Response (..),
    ResponseBody (..),
    Status (Status),
 )
import Cloudflare.Workers.Reactor (WorkersExecutionContext (WorkersExecutionContext))
import Cloudflare.Workers.URL (parseURL)
import Control.Monad (forM_, unless)
import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy qualified as Lazy
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Time (UTCTime)
import Presentation.API (ProofreadResponse (..), RegenerationResponse (..))
import Presentation.Handler.API.Metadata (MetadataDependencies (..))
import Presentation.Handler.API.Proofread (ProofreadHandlerDependencies (..))
import Presentation.Handler.API.RequestExcerptRegeneration (
    RegenerationHandlerDependencies (..),
 )
import Presentation.Server.API (APIServerDependencies (..), articleAPIServer)
import Shared.Domain.Error (
    DomainError,
    createAggregateNotFound,
    createInvariantViolation,
    createOperationNotAllowed,
    createProcessingTargetChanged,
    createServiceUnavailable,
    createTransactionOutcomeUnknown,
    createUnexpectedError,
 )
import Shared.UseCase.Command (
    Command (..),
    actorText,
    correlationIdentifierText,
 )
import "article" Domain.Article.Common (articleIdentifierText)
import "article" UseCase.Proofread (ProofreadPayload (..))

articleIdentifier :: Text
articleIdentifier = "01ARZ3NDEKTSV4RRFFQ69G5FAV"

correlationIdentifier :: Text
correlationIdentifier = "01ARZ3NDEKTSV4RRFFQ69G5FAX"

fixedTime :: UTCTime
fixedTime = read "2026-01-01 00:00:00 UTC"

check :: String -> Bool -> IO ()
check label condition = unless condition (fail label)

run :: IO ()
run = do
    proofreadSuccess
    regenerateSuccess
    invalidInputs
    mapsDomainErrors
    metadataFailures
    routeAndMethod

proofreadSuccess :: IO ()
proofreadSuccess = do
    received <- newIORef Nothing
    let dependencies = baseDependencies
            { proofread = ProofreadHandlerDependencies metadata $ \command -> do
                writeIORef received (Just command)
                pure (Right ())
            }
    response <- send dependencies POST
        ("/admin/articles/" <> articleIdentifier <> "/proofreading")
        [("X-Hut-Actor", "editor"), ("X-Correlation-Identifier", correlationIdentifier)]
    check "proofread returns 200" (status response == 200)
    check "proofread response" (bodyJSON response == Just (ProofreadResponse articleIdentifier "proofreaded"))
    check "correlation returned" $
        headerLookup "X-Correlation-Identifier" response.responseHeaders
            == Just correlationIdentifier
    command <- readIORef received
    check "command uses supplied metadata" $ case command of
        Just value ->
            value.timestamp == fixedTime
                && actorText value.actor == "editor"
                && correlationIdentifierText value.correlation == correlationIdentifier
                && articleIdentifierText value.payload.article == articleIdentifier
        Nothing -> False

regenerateSuccess :: IO ()
regenerateSuccess = do
    response <- send baseDependencies POST
        ("/admin/articles/" <> articleIdentifier <> "/excerpt-generation-requests")
        [("X-Hut-Actor", "editor")]
    check "regeneration returns 200" (status response == 200)
    check "regeneration response" $ bodyJSON response == Just
        (RegenerationResponse articleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    check "generated correlation returned" $
        headerLookup "X-Correlation-Identifier" response.responseHeaders == Just correlationIdentifier

invalidInputs :: IO ()
invalidInputs = do
    let path = "/admin/articles/" <> articleIdentifier <> "/proofreading"
    missingActor <- send baseDependencies POST path []
    check "missing actor rejected" (status missingActor == 400)
    check "actor error code" (errorCode missingActor == Just "invalid_actor")
    badIdentifier <- send baseDependencies POST
        "/admin/articles/not-an-identifier/proofreading"
        [("X-Hut-Actor", "editor")]
    check "invalid article identifier rejected" (status badIdentifier == 400)
    check "article error code" (errorCode badIdentifier == Just "invalid_article_identifier")
    badCorrelation <- send baseDependencies POST path
        [("X-Hut-Actor", "editor"), ("X-Correlation-Identifier", "bad")]
    check "invalid correlation rejected" (status badCorrelation == 400)
    check "correlation error code" (errorCode badCorrelation == Just "invalid_correlation")

mapsDomainErrors :: IO ()
mapsDomainErrors = do
    let missing = baseDependencies
            { proofread = ProofreadHandlerDependencies metadata
                (const (pure (Left (createAggregateNotFound "Article" "missing"))))
            }
        unavailable = baseDependencies
            { requestExcerptRegeneration = RegenerationHandlerDependencies metadata
                (const (pure (Left (createServiceUnavailable "Article" "unavailable"))))
            }
        headers = [("X-Hut-Actor", "editor")]
    notFound <- send missing POST
        ("/admin/articles/" <> articleIdentifier <> "/proofreading") headers
    check "missing article returns 404" (status notFound == 404)
    check "missing article code" (errorCode notFound == Just "article_not_found")
    failed <- send unavailable POST
        ("/admin/articles/" <> articleIdentifier <> "/excerpt-generation-requests") headers
    check "dependency failure returns 503" (status failed == 503)
    check "dependency failure code" (errorCode failed == Just "service_unavailable")
    forM_
        [ (createInvariantViolation "Article" "invalid", 400, "invalid_article")
        , (createOperationNotAllowed "Article" "wrong phase", 409, "operation_not_allowed")
        , (createProcessingTargetChanged "Article" "changed", 409, "processing_target_changed")
        , (createTransactionOutcomeUnknown "Article" "unknown", 500, "transaction_outcome_unknown")
        , (createUnexpectedError "Article" "unexpected", 500, "unexpected_error")
        ] $ \(failure, expectedStatus, expectedCode) -> do
            let dependencies = withProofreadError failure
            actual <- send dependencies POST
                ("/admin/articles/" <> articleIdentifier <> "/proofreading") headers
            check "mapped domain status" (status actual == expectedStatus)
            check "mapped domain code" (errorCode actual == Just expectedCode)

metadataFailures :: IO ()
metadataFailures = do
    let path = "/admin/articles/" <> articleIdentifier <> "/proofreading"
        actorHeader = [("X-Hut-Actor", "editor")]
        unavailable = createServiceUnavailable "Metadata" "unavailable"
        missingTime = baseDependencies
            { proofread = ProofreadHandlerDependencies
                metadata{currentTime = pure (Left unavailable)}
                (const (pure (Right ())))
            }
        missingCorrelation = baseDependencies
            { proofread = ProofreadHandlerDependencies
                metadata{newCorrelation = pure (Left unavailable)}
                (const (pure (Right ())))
            }
    timeResponse <- send missingTime POST path actorHeader
    check "timestamp unavailable" (status timeResponse == 503)
    check "timestamp error code" (errorCode timeResponse == Just "timestamp_unavailable")
    correlationResponse <- send missingCorrelation POST path actorHeader
    check "correlation unavailable" (status correlationResponse == 503)
    check "correlation error code" $
        errorCode correlationResponse == Just "correlation_unavailable"
    badRegeneration <- send baseDependencies POST
        "/admin/articles/not-an-identifier/excerpt-generation-requests" actorHeader
    check "regeneration checks identifier" (status badRegeneration == 400)
    check "regeneration identifier code" $
        errorCode badRegeneration == Just "invalid_article_identifier"

withProofreadError :: DomainError -> APIServerDependencies
withProofreadError failure = baseDependencies
    { proofread = ProofreadHandlerDependencies metadata
        (const (pure (Left failure)))
    }

routeAndMethod :: IO ()
routeAndMethod = do
    let path = "/admin/articles/" <> articleIdentifier <> "/proofreading"
        headers = [("X-Hut-Actor", "editor")]
    wrongMethod <- send baseDependencies GET path headers
    check "unsupported method rejected" (status wrongMethod == 405)
    unknown <- send baseDependencies POST "/admin/unknown" headers
    check "unknown path rejected" (status unknown == 404)

metadata :: MetadataDependencies
metadata = MetadataDependencies
    { currentTime = pure (Right fixedTime)
    , newCorrelation = pure (Right correlationIdentifier)
    }

baseDependencies :: APIServerDependencies
baseDependencies = APIServerDependencies
    { proofread = ProofreadHandlerDependencies metadata (const (pure (Right ())))
    , requestExcerptRegeneration = RegenerationHandlerDependencies metadata
        (const (pure (Right "01ARZ3NDEKTSV4RRFFQ69G5FAW")))
    }

send :: APIServerDependencies -> Method -> Text -> [(Text, Text)] -> IO Response
send dependencies method path headers =
    articleAPIServer
        (Request method url Nothing (headersFromList headers) Nothing Nothing)
        dependencies
        (WorkersExecutionContext phantomJSVal)
  where
    url = maybe (error "invalid test URL") id
        (parseURL ("https://article.example.test" <> path))

status :: Response -> Int
status response = case response.responseStatus of Status code -> code

errorCode :: Response -> Maybe Text
errorCode response = headerLookup "X-Article-Error-Code" response.responseHeaders

bodyJSON :: (FromJSON value) => Response -> Maybe value
bodyJSON response = case response.responseBody of
    ResponseBodyBytes bytes -> decode (Lazy.fromStrict bytes)
    ResponseBodyLazyBytes bytes -> decode bytes
    _ -> Nothing
