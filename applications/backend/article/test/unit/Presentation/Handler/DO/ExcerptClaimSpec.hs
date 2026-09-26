module Presentation.Handler.DO.ExcerptClaimSpec (run) where

import Article.Worker.Excerpt.Contract (decodeClaimedContent)
import Cloudflare.Workers.Binding.DurableObject.SQL (
    SQLResult (..),
    SQLStatement (..),
    SQLValue (..),
 )
import Cloudflare.Workers.Headers (headerLookup, headersFromList)
import Cloudflare.Workers.HTTP (
    Method (GET, POST),
    Request (..),
    Response (..),
    ResponseBody (ResponseBodyBytes),
    Status (Status),
 )
import Cloudflare.Workers.URL (parseURL)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Article (Article (..), articleIdentifierText)
import Domain.Article.Draft (proofread, proofreadedContent)
import Infrastructure.Article.DurableObject.Codec (articleCodec)
import Infrastructure.Article.DurableObject.Repository (ArticleCodec (..), ExecuteSQL)
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerationRequested (..),
    generationRequestIdentifierText,
    newGenerationRequestIdentifier,
 )
import Presentation.Handler.DO.ExcerptClaim (handleExcerptClaim)
import Shared.Domain.Common.Primitive (newPositiveInteger)
import Shared.Infrastructure.Versioning (newVersion)
import TestSupport (check, confirmed, identifier, right, start, timestamp)

articleText :: Text
articleText = "01ARZ3NDEKTSV4RRFFQ69G5FAV"

requestText :: Text
requestText = "01ARZ3NDEKTSV4RRFFQ69G5FAW"

claimPath :: Text
claimPath =
    "https://article.internal/internal/excerpt-generation/claim"
        <> "?article=" <> articleText
        <> "&request=" <> requestText
        <> "&expectedRevision=3"

requestAt :: Method -> Text -> IO Request
requestAt method location = do
    url <- maybe (fail "test URL is invalid") pure (parseURL location)
    pure Request
        { requestMethodField = method
        , requestURLField = url
        , requestBodyField = Nothing
        , requestHeaders = headersFromList []
        , requestBodyReaderField = Nothing
        , requestDataCenterField = Nothing
        }

generationRequest :: IO ExcerptGenerationRequested
generationRequest = do
    article <- right identifier
    request <- right (newGenerationRequestIdentifier requestText)
    revision <- newVersion <$> right (newPositiveInteger 3)
    pure (ExcerptGenerationRequested request article revision)

emptyResult :: SQLResult
emptyResult = SQLResult [] [] 0 0

oneRow :: [SQLValue] -> SQLResult
oneRow row = SQLResult [] [row] 0 1

script :: [SQLResult] -> IO (ExecuteSQL, IO [SQLStatement])
script results = do
    remaining <- newIORef results
    issued <- newIORef []
    let execute statement = do
            modifyIORef' issued (<> [statement])
            next <- readIORef remaining
            case next of
                [] -> fail "unexpected SQL statement"
                result : rest -> do
                    modifyIORef' remaining (const rest)
                    pure result
    pure (execute, readIORef issued)

withinTransaction :: IORef Int -> IO value -> IO value
withinTransaction count action = do
    modifyIORef' count (+ 1)
    action

statusIs :: Int -> Response -> Bool
statusIs code response = response.responseStatus == Status code

bodyIsEmpty :: Response -> Bool
bodyIsEmpty response = case response.responseBody of
    ResponseBodyBytes bytes -> bytes == ""
    _ -> False

run :: IO ()
run = do
    servesProofreadedContent
    acknowledgesObsoleteAndNonProofreaded
    rejectsInvalidInput
    handlesStorageFailure

servesProofreadedContent :: IO ()
servesProofreadedContent = do
    generation <- generationRequest
    original <- right start
    available <- right confirmed
    draft <- right (proofread (timestamp 1) available original)
    encoded <- right (articleCodec.encodeArticle (Proofreaded draft))
    (execute, issued) <- script
        [ oneRow [SQLText requestText]
        , oneRow [SQLText "haskell-syntax", SQLText encoded, SQLNumber 3]
        ]
    count <- newIORef 0
    request <- requestAt GET claimPath
    response <- handleExcerptClaim (withinTransaction count) execute request
    check "valid claim returns 200" (statusIs 200 response)
    check "claim response is JSON" $
        headerLookup "content-type" response.responseHeaders
            == Just "application/json; charset=utf-8"
    check "claim response cannot be cached" $
        headerLookup "cache-control" response.responseHeaders == Just "no-store"
    case response.responseBody of
        ResponseBodyBytes bytes -> do
            claimed <- right (decodeClaimedContent generation bytes)
            check "claim contains proofreaded body" (claimed == proofreadedContent draft)
        _ -> fail "claim must return a byte body"
    check "both reads share one transaction" . (== 1) =<< readIORef count
    statements <- issued
    check "claim and article each queried once" (length statements == 2)
    case statements of
        [claim, article] -> do
            check "claim binds active request" $
                SQLText (generationRequestIdentifierText generation.identifier)
                    `elem` claim.parameters
            check "article query binds the same article" $
                article.parameters == [SQLText (articleIdentifierText generation.article)]
        _ -> fail "expected claim and article query"

acknowledgesObsoleteAndNonProofreaded :: IO ()
acknowledgesObsoleteAndNonProofreaded = do
    request <- requestAt GET claimPath
    count <- newIORef 0
    (obsoleteSQL, obsoleteIssued) <- script [emptyResult]
    obsolete <- handleExcerptClaim (withinTransaction count) obsoleteSQL request
    check "obsolete request is acknowledged" (statusIs 204 obsolete && bodyIsEmpty obsolete)
    check "obsolete claim does not read the article" . (== 1) . length =<< obsoleteIssued

    draft <- right start
    encoded <- right (articleCodec.encodeArticle (Unvalidated draft))
    (execute, _) <- script
        [ oneRow [SQLText requestText]
        , oneRow [SQLText "haskell-syntax", SQLText encoded, SQLNumber 3]
        ]
    nonProofreaded <- handleExcerptClaim (withinTransaction count) execute request
    check "non-proofreaded article is obsolete" $
        statusIs 204 nonProofreaded && bodyIsEmpty nonProofreaded

rejectsInvalidInput :: IO ()
rejectsInvalidInput = do
    count <- newIORef 0
    let unusedSQL _ = fail "invalid input must not read storage"
        invalid =
            [ (GET, Text.replace "&request=" "&request=bad&request=" claimPath)
            , (GET, Text.replace "&expectedRevision=3" "&expectedRevision=0" claimPath)
            , (GET, Text.replace "&expectedRevision=3" "&expectedRevision=-1" claimPath)
            , (GET, Text.replace "&expectedRevision=3" "&expectedRevision=9007199254740992" claimPath)
            , (GET, Text.replace "&expectedRevision=3" "&expectedRevision=00000000000000003" claimPath)
            , (GET, Text.replace "?article=" "?article=bad&article=" claimPath)
            , (GET, Text.replace "&request=" "&missing=" claimPath)
            , (GET, Text.replace articleText "invalid" claimPath)
            , (GET, Text.replace "/claim?" "/other?" claimPath)
            , (POST, claimPath)
            ]
    mapM_ (checkInvalid count unusedSQL) invalid
    check "bad requests never open a transaction" . (== 0) =<< readIORef count

checkInvalid :: IORef Int -> ExecuteSQL -> (Method, Text) -> IO ()
checkInvalid count execute (method, location) = do
    request <- requestAt method location
    response <- handleExcerptClaim (withinTransaction count) execute request
    check ("invalid claim returns 400 without content: " <> Text.unpack location) $
        statusIs 400 response && bodyIsEmpty response

handlesStorageFailure :: IO ()
handlesStorageFailure = do
    request <- requestAt GET claimPath
    count <- newIORef 0
    let failingSQL _ = fail "storage failure"
    failure <- handleExcerptClaim (withinTransaction count) failingSQL request
    check "SQL exception returns 500" (statusIs 500 failure && bodyIsEmpty failure)
    (corruptSQL, _) <- script
        [ oneRow [SQLText requestText]
        , oneRow [SQLText "haskell-syntax", SQLText "not-json", SQLNumber 3]
        ]
    corrupt <- handleExcerptClaim (withinTransaction count) corruptSQL request
    check "corrupt article row returns 500" (statusIs 500 corrupt && bodyIsEmpty corrupt)
