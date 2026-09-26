{-# LANGUAGE PackageImports #-}

module Presentation.Server.APISpec (run) where

import Cloudflare.Workers.Headers (headerLookup, headersFromList)
import Cloudflare.Workers.HostTestKit (phantomJSVal)
import Cloudflare.Workers.HTTP (
    Method (DELETE, GET, PATCH, POST, PUT),
    Request (..),
    Response (..),
    ResponseBody (..),
    Status (Status),
 )
import Cloudflare.Workers.Reactor (WorkersExecutionContext (WorkersExecutionContext))
import Cloudflare.Workers.URL (parseURL)
import Control.Monad (forM_, unless)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy qualified as Lazy
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import Presentation.API (
    DiscardResponse (..),
    DraftRequest (..),
    ExcerptRequest (..),
    ProofreadResponse (..),
    RegenerationResponse (..),
    SlugAvailabilityResponse (..),
 )
import Presentation.API.ArticleView (ArticlePage (..), ArticleView (..), PageView (..))
import Presentation.Handler.API.Metadata (MetadataDependencies (..))
import Presentation.Handler.API.Proofread (ProofreadHandlerDependencies (..))
import Presentation.Handler.API.DraftWriting (DraftWritingDependencies (..))
import Presentation.Handler.API.Publication (PublicationHandlerDependencies (..))
import Presentation.Handler.API.Reading (ReadingHandlerDependencies (..))
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
import Shared.Domain.Event (Events (..))
import Shared.Domain.Pager (newPager)
import Shared.UseCase.Command (
    Command (..),
    actorText,
    correlationIdentifierText,
 )
import "article" Domain.Article (Article (..), articleIdentifierText)
import "article" Domain.Article.Criteria (ArticleFilter (..))
import "article" Domain.Article.Common (
    DraftInput (..),
    confirmAvailableImageReferences,
    newArticleIdentifier,
    newDraftContent,
 )
import "article" Domain.Article.Draft (
    ProofreadedDraft,
    ReadyToPublish,
    newUnvalidatedDraft,
    prepareToPublish,
    proofread,
 )
import "article" Domain.Article.Private (PrivateArticle, takeDown)
import "article" Domain.Article.Published (PublishedArticle, publish)
import "article" UseCase.BrowseArticlesForAdmin qualified as BrowseAdmin
import "article" UseCase.BrowseArticlesForReader qualified as BrowseReader
import "article" UseCase.CheckSlugAvailability qualified as CheckSlug
import "article" UseCase.AmendDraft (AmendDraftPayload (..))
import "article" UseCase.DiscardArticle (DiscardArticlePayload (..))
import "article" UseCase.Publish (PublishPayload (..))
import "article" UseCase.ReadArticle qualified as ReadArticle
import "article" UseCase.ResumePublication (ResumePublicationPayload (..))
import "article" UseCase.TakeDown (TakeDownPayload (..))
import "article" UseCase.ViewArticleForAdmin qualified as ViewAdmin
import "article" UseCase.PrepareToPublish (PrepareToPublishPayload (..))
import Shared.Domain.Excerpt (newExcerpt)
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
    readingSuccess
    readingFilters
    otherViews
    invalidReadingInputs
    writingSuccess
    operationFailures
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

readingSuccess :: IO ()
readingSuccess = do
    draft <- draftFixture
    publication <- publishedFixture
    pager <- right (newPager 1 10 1)
    let reading = (baseDependencies.reading)
            { browseAdmin = \command -> do
                check "admin browse command" (command.payload.current == 1
                    && command.payload.items == Just 10
                    && command.payload.status == UnvalidatedOnly)
                pure (Right (BrowseAdmin.BrowseArticlesForAdminResult [draft] pager (Events [])))
            , viewAdmin = \command -> do
                check "admin view identifier"
                    (articleIdentifierText command.payload.article == articleIdentifier)
                pure (Right (ViewAdmin.ViewArticleForAdminResult draft (Events [])))
            , checkSlug = \command -> do
                check "slug command" (command.payload.slug == "haskell-syntax"
                    && articleIdentifierText command.payload.article == articleIdentifier)
                pure (Right (CheckSlug.CheckSlugAvailabilityResult CheckSlug.Available (Events [])))
            , browseReader = \command -> do
                check "reader browse command" (command.payload.current == 1
                    && command.payload.items == Just 10)
                pure (Right (BrowseReader.BrowseArticlesForReaderResult [publication] pager (Events [])))
            , readArticle = \command -> do
                check "reader slug command" (command.payload.slug == "haskell-syntax")
                pure (Right (ReadArticle.ReadArticleResult publication (Events [])))
            }
        dependencies = baseDependencies{reading}
        headers = [("X-Hut-Actor", "editor")]
    adminPage <- send dependencies GET
        "/admin/articles?page=1&size=10&status=unvalidated" headers
    check "admin list returns 200" (status adminPage == 200)
    checkCorrelation adminPage
    check "admin list has full article" (case bodyJSON adminPage :: Maybe ArticlePage of
        Just page -> case page.articles of
            [article] -> article.phase == "unvalidated"
                && article.body == "Article body"
                && page.pagination == PageView 1 10 1 1 1
            _ -> False
        Nothing -> False)
    adminDetail <- send dependencies GET
        ("/admin/articles/" <> articleIdentifier) headers
    checkCorrelation adminDetail
    check "admin detail returns draft" (case bodyJSON adminDetail :: Maybe ArticleView of
        Just article -> article.phase == "unvalidated"
        Nothing -> False)
    slug <- send dependencies GET
        ("/admin/articles/" <> articleIdentifier <> "/slug-availability?slug=haskell-syntax") headers
    checkCorrelation slug
    check "slug availability returns 200" (status slug == 200)
    check "slug availability response" (case bodyJSON slug :: Maybe SlugAvailabilityResponse of
        Just value -> value.available
        Nothing -> False)
    let occupied = reading
            { checkSlug = const (pure (Right
                (CheckSlug.CheckSlugAvailabilityResult CheckSlug.InUse (Events [])))) }
    inUse <- send dependencies{reading = occupied} GET
        ("/admin/articles/" <> articleIdentifier <> "/slug-availability?slug=occupied") headers
    check "occupied slug is unavailable" (bodyJSON inUse == Just
        (SlugAvailabilityResponse False))
    readerPage <- send dependencies GET "/articles?page=1&size=10" []
    checkCorrelation readerPage
    check "reader list needs no admin actor" (status readerPage == 200)
    check "reader list exposes only published view" (case bodyJSON readerPage :: Maybe ArticlePage of
        Just page -> case page.articles of
            [article] -> article.phase == "published"
            _ -> False
        Nothing -> False)
    readerDetail <- send dependencies GET "/articles/haskell-syntax" []
    checkCorrelation readerDetail
    check "reader detail returns published article" (case bodyJSON readerDetail :: Maybe ArticleView of
        Just article -> article.phase == "published" && article.slug == Just "haskell-syntax"
        Nothing -> False)

invalidReadingInputs :: IO ()
invalidReadingInputs = do
    let headers = [("X-Hut-Actor", "editor")]
    badPage <- send baseDependencies GET "/admin/articles?page=0" headers
    check "page zero is rejected" (status badPage == 400)
    check "page zero code" (errorCode badPage == Just "invalid_page")
    badSize <- send baseDependencies GET "/articles?size=-2" []
    check "negative reader page size is rejected" (status badSize == 400)
    check "negative size code" (errorCode badSize == Just "invalid_size")
    badStatus <- send baseDependencies GET "/admin/articles?status=unknown" headers
    check "unknown state is rejected" (status badStatus == 400)
    check "unknown state code" (errorCode badStatus == Just "invalid_status")
    missingSlug <- send baseDependencies GET
        ("/admin/articles/" <> articleIdentifier <> "/slug-availability") headers
    check "missing slug is rejected" (status missingSlug == 400)
    check "missing slug code" (errorCode missingSlug == Just "missing_slug")
    invalidIdentifier <- send baseDependencies GET "/admin/articles/invalid" headers
    check "bad admin identifier is rejected" (status invalidIdentifier == 400)
    badPageText <- send baseDependencies GET "/admin/articles?page=1.2" headers
    check "fractional page rejected" (status badPageText == 400)
    hugePage <- send baseDependencies GET "/admin/articles?page=999999999999999999999999" headers
    check "overflowing page rejected" (status hugePage == 400)
    badReaderPage <- send baseDependencies GET "/articles?page=abc" []
    check "bad reader page rejected" (status badReaderPage == 400)
    check "bad reader page code" (errorCode badReaderPage == Just "invalid_page")
    unicodePage <- send baseDependencies GET "/articles?page=１２" []
    check "non-ASCII digits are rejected" (status unicodePage == 400)
    overflowingInt <- send baseDependencies GET
        "/articles?page=9223372036854775808" []
    check "Int overflow is rejected" (status overflowingInt == 400)

readingFilters :: IO ()
readingFilters = do
    pager <- right (newPager 0 10 1)
    let headers = [("X-Hut-Actor", "editor")]
        filters = [ ("all", AllArticles)
            , ("proofreaded", ProofreadedOnly)
            , ("ready", ReadyOnly)
            , ("published", PublishedOnly)
            , ("private", PrivateOnly)
            ]
    forM_ filters $ \(rawStatus, expected) -> do
        let reading = baseDependencies.reading
                { browseAdmin = \command -> do
                    check "admin filter command" (command.payload.status == expected
                        && command.payload.current == 1
                        && command.payload.items == Nothing)
                    pure (Right (BrowseAdmin.BrowseArticlesForAdminResult [] pager (Events [])))
                }
        response <- send baseDependencies{reading} GET
            ("/admin/articles?status=" <> rawStatus) headers
        check "admin filter response" (status response == 200)
    let reader = baseDependencies.reading
            { browseReader = \command -> do
                check "reader defaults" (command.payload.current == 1
                    && command.payload.items == Nothing)
                pure (Right (BrowseReader.BrowseArticlesForReaderResult [] pager (Events [])))
            }
    response <- send baseDependencies{reading = reader} GET "/articles" []
    check "reader defaults response" (status response == 200)

otherViews :: IO ()
otherViews = do
    proofreaded <- proofreadedFixture
    ready <- readyFixture
    publication <- publishedFixture
    private <- privateFixture
    let headers = [("X-Hut-Actor", "editor")]
        path = "/admin/articles/" <> articleIdentifier
    forM_ [(Proofreaded proofreaded, "proofreaded"),
            (Ready ready, "ready"), (Private private, "private"),
            (Published publication, "published")] $ \(article, expected) -> do
        let reading = baseDependencies.reading
                { viewAdmin = const (pure (Right
                    (ViewAdmin.ViewArticleForAdminResult article (Events [])))) }
        response <- send baseDependencies{reading} GET path headers
        check "article view phase" (hasPhase expected response)
        check "article view keeps publication timestamp" (case bodyJSON response :: Maybe ArticleView of
            Just value -> (expected == "published" || expected == "private")
                == (value.publishedAt /= Nothing)
            Nothing -> False)

right :: (Show errorValue) => Either errorValue value -> IO value
right = either (fail . show) pure

draftFixture :: IO Article
draftFixture = do
    identifier <- right (newArticleIdentifier articleIdentifier)
    content <- right (newDraftContent (const (Right Set.empty))
        (DraftInput "Fixture" "Article body" (Just "haskell-syntax") []))
    Unvalidated <$> right (newUnvalidatedDraft identifier fixedTime content)

publishedFixture :: IO PublishedArticle
publishedFixture = do
    ready <- readyFixture
    right (publish fixedTime ready)

readyFixture :: IO ReadyToPublish
readyFixture = do
    draft <- draftFixture
    case draft of
        Unvalidated value -> do
            available <- right (confirmAvailableImageReferences Set.empty Set.empty)
            proofreaded <- right (proofread fixedTime available value)
            excerpt <- right (newExcerpt "Summary")
            right (prepareToPublish fixedTime excerpt proofreaded)
        _ -> fail "expected draft fixture"

privateFixture :: IO PrivateArticle
privateFixture = publishedFixture >>= right . takeDown fixedTime

proofreadedFixture :: IO ProofreadedDraft
proofreadedFixture = do
    draft <- draftFixture
    case draft of
        Unvalidated value -> do
            available <- right (confirmAvailableImageReferences Set.empty Set.empty)
            right (proofread fixedTime available value)
        _ -> fail "expected draft fixture"

writingSuccess :: IO ()
writingSuccess = do
    draft <- draftFixture
    ready <- readyFixture
    publication <- publishedFixture
    private <- privateFixture
    let Unvalidated unvalidated = draft
        identifier = publication.identifier
        writing = (baseDependencies.draftWriting)
            { jotDown = \command -> do
                check "JotDown input" (command.payload.title == "Fixture"
                    && command.payload.body == "Article body"
                    && command.payload.slug == Just "haskell-syntax"
                    && command.payload.tags == [])
                check "JotDown actor" (actorText command.actor == "editor")
                pure (Right unvalidated)
            , amendDraft = \command -> do
                check "AmendDraft input" (command.payload.title == "Fixture"
                    && articleIdentifierText command.payload.article == articleIdentifier)
                pure (Right unvalidated)
            , reviseExcerpt = \command -> do
                check "ReviseExcerpt input" (case command.payload of
                    ReviseExcerpt article excerpt ->
                        articleIdentifierText article == articleIdentifier
                            && excerpt == "Updated excerpt"
                    _ -> False)
                pure (Right ready)
            }
        transitions = (baseDependencies.publication)
            { publish = \command -> do
                check "Publish target"
                    (articleIdentifierText command.payload.article == articleIdentifier)
                pure (Right publication)
            , takeDown = \command -> do
                check "TakeDown target"
                    (articleIdentifierText command.payload.article == articleIdentifier)
                pure (Right private)
            , resumePublication = \command -> do
                check "ResumePublication target"
                    (articleIdentifierText command.payload.article == articleIdentifier)
                pure (Right ready)
            , discardArticle = \command -> do
                check "DiscardArticle target"
                    (articleIdentifierText command.payload.article == articleIdentifier)
                pure (Right identifier)
            }
        dependencies = baseDependencies{draftWriting = writing, publication = transitions}
        headers = [("X-Hut-Actor", "editor")]
        input = DraftRequest "Fixture" "Article body" (Just "haskell-syntax") []
        path = "/admin/articles/" <> articleIdentifier
    started <- sendJSON dependencies POST "/admin/articles" headers input
    check "JotDown returns 201" (status started == 201)
    check "JotDown returns draft" (hasPhase "unvalidated" started)
    check "JotDown correlation" (headerLookup "X-Correlation-Identifier"
        started.responseHeaders == Just correlationIdentifier)
    amended <- sendJSON dependencies PUT (path <> "/draft") headers input
    check "AmendDraft returns draft" (hasPhase "unvalidated" amended)
    check "AmendDraft correlation" (headerLookup "X-Correlation-Identifier"
        amended.responseHeaders == Just correlationIdentifier)
    revised <- sendJSON dependencies PATCH (path <> "/excerpt") headers
        (ExcerptRequest "Updated excerpt")
    check "excerpt revision returns ready draft" (hasPhase "ready" revised)
    check "excerpt revision correlation" (headerLookup "X-Correlation-Identifier"
        revised.responseHeaders == Just correlationIdentifier)
    published <- send dependencies POST (path <> "/publication") headers
    check "Publish returns published" (hasPhase "published" published)
    checkCorrelation published
    takenDown <- send dependencies DELETE (path <> "/publication") headers
    check "TakeDown returns private" (hasPhase "private" takenDown)
    checkCorrelation takenDown
    resumed <- send dependencies POST (path <> "/publication-resumptions") headers
    check "ResumePublication returns ready" (hasPhase "ready" resumed)
    checkCorrelation resumed
    discarded <- send dependencies DELETE path headers
    check "DiscardArticle returns identifier" (bodyJSON discarded
        == Just (DiscardResponse articleIdentifier))
    checkCorrelation discarded

operationFailures :: IO ()
operationFailures = do
    let failure = createOperationNotAllowed "Article" "wrong phase"
        path = "/admin/articles/" <> articleIdentifier
        headers = [("X-Hut-Actor", "editor")]
        input = DraftRequest "Title" "Body" (Just "article-slug") []
        writing = baseDependencies.draftWriting
            { jotDown = rejectAdmin
            , amendDraft = rejectAdmin
            , reviseExcerpt = rejectAdmin
            }
        publication = baseDependencies.publication
            { publish = rejectAdmin
            , takeDown = rejectAdmin
            , resumePublication = rejectAdmin
            , discardArticle = rejectAdmin
            }
        reading = baseDependencies.reading
            { browseAdmin = rejectAdmin
            , viewAdmin = rejectAdmin
            , checkSlug = rejectAdmin
            , browseReader = rejectReader
            , readArticle = rejectReader
            }
        dependencies = baseDependencies
            { draftWriting = writing, publication, reading }
        rejectAdmin command = reject "editor" command
        rejectReader command = reject "reader" command
        reject expectedActor command = do
            check "failed operation command metadata"
                (command.timestamp == fixedTime
                    && actorText command.actor == expectedActor
                    && correlationIdentifierText command.correlation == correlationIdentifier
                    && length (show command.payload) > 0)
            pure (Left failure)
    forM_
        [ sendJSON dependencies POST "/admin/articles" headers input
        , sendJSON dependencies PUT (path <> "/draft") headers input
        , sendJSON dependencies PATCH (path <> "/excerpt") headers (ExcerptRequest "summary")
        , send dependencies POST (path <> "/publication") headers
        , send dependencies DELETE (path <> "/publication") headers
        , send dependencies POST (path <> "/publication-resumptions") headers
        , send dependencies DELETE path headers
        , send dependencies GET "/admin/articles" headers
        , send dependencies GET path headers
        , send dependencies GET (path <> "/slug-availability?slug=article-slug") headers
        , send dependencies GET "/articles" []
        , send dependencies GET "/articles/article-slug" []
        ] $ \action -> do
            response <- action
            check "use-case failure returns conflict" (status response == 409)
            check "use-case failure exposes stable code"
                (errorCode response == Just "operation_not_allowed")
            check "use-case failure preserves correlation"
                (headerLookup "X-Correlation-Identifier" response.responseHeaders
                    == Just correlationIdentifier)
    invalidDraft <- sendJSON baseDependencies PUT
        "/admin/articles/invalid/draft" headers input
    check "draft update validates identifier" (status invalidDraft == 400)
    invalidPublication <- send baseDependencies POST
        "/admin/articles/invalid/publication" headers
    check "publication validates identifier" (status invalidPublication == 400)

hasPhase :: Text -> Response -> Bool
hasPhase phase response = case bodyJSON response :: Maybe ArticleView of
    Just view -> status response >= 200 && status response < 300 && view.phase == phase
    Nothing -> False

checkCorrelation :: Response -> IO ()
checkCorrelation response = check "response correlation"
    (headerLookup "X-Correlation-Identifier" response.responseHeaders
        == Just correlationIdentifier)

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
    , reading = ReadingHandlerDependencies
        { metadata
        , browseAdmin = unavailable
        , viewAdmin = unavailable
        , checkSlug = unavailable
        , browseReader = unavailable
        , readArticle = unavailable
        }
    , draftWriting = DraftWritingDependencies
        { metadata
        , jotDown = unavailable
        , amendDraft = unavailable
        , reviseExcerpt = unavailable
        }
    , publication = PublicationHandlerDependencies
        { metadata
        , publish = unavailable
        , takeDown = unavailable
        , resumePublication = unavailable
        , discardArticle = unavailable
        }
    }
  where
    unavailable _ = pure (Left (createUnexpectedError "Test" "unconfigured reader"))

send :: APIServerDependencies -> Method -> Text -> [(Text, Text)] -> IO Response
send dependencies method path headers =
    articleAPIServer
        (Request method url Nothing (headersFromList headers) Nothing Nothing)
        dependencies
        (WorkersExecutionContext phantomJSVal)
  where
    url = maybe (error "invalid test URL") id
        (parseURL ("https://article.example.test" <> path))

sendJSON :: (ToJSON body) =>
    APIServerDependencies -> Method -> Text -> [(Text, Text)] -> body -> IO Response
sendJSON dependencies method path headers body =
    articleAPIServer
        (Request method url Nothing requestHeaders
            (Just (\_ -> pure (Right (encode body)))) Nothing)
        dependencies
        (WorkersExecutionContext phantomJSVal)
  where
    url = maybe (error "invalid test URL") id
        (parseURL ("https://article.example.test" <> path))
    requestHeaders = headersFromList (("content-type", "application/json") : headers)

status :: Response -> Int
status response = case response.responseStatus of Status code -> code

errorCode :: Response -> Maybe Text
errorCode response = headerLookup "X-Article-Error-Code" response.responseHeaders

bodyJSON :: (FromJSON value) => Response -> Maybe value
bodyJSON response = case response.responseBody of
    ResponseBodyBytes bytes -> decode (Lazy.fromStrict bytes)
    ResponseBodyLazyBytes bytes -> decode bytes
    _ -> Nothing
