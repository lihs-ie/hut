{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.DurableObject.QuerySpec (run) where

import Cloudflare.Workers.Binding.DurableObject.SQL (
    SQLError (..),
    SQLResult (..),
    SQLStatement (..),
    SQLValue (..),
 )
import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (UTCTime)
import "article" Domain.Article (
    Article (..),
    ArticleIdentifier,
    articleIdentifier,
    articleIdentifierText,
    newArticleIdentifier,
 )
import "article" Domain.Article.Common (
    DraftInput (..),
    confirmAvailableImageReferences,
    newDraftContent,
 )
import "article" Domain.Article.Criteria (ArticleFilter (..), newCriteria, newReaderCriteria)
import "article" Domain.Article.Draft (
    newUnvalidatedDraft,
    prepareToPublish,
    proofread,
 )
import "article" Domain.Article.Published (publish)
import Infrastructure.Article.DurableObject.Codec (articleCodec)
import Infrastructure.Article.DurableObject.Query
import Infrastructure.Article.DurableObject.Repository (ArticleCodec (..), ExecuteSQL)
import "shared" Shared.Domain.Error (DomainError (..))
import "shared" Shared.Domain.Excerpt (newExcerpt)
import "shared" Shared.Domain.Slug (newSlug)
import "shared" Shared.Domain.Tag (newTagIdentifier)
import "shared" Shared.Infrastructure.Versioning (VersionContext, emptyVersionContext)

run :: IO ()
run = do
    readsCatalogSnapshot
    findsSlugOwner
    findsArticleBySlug
    searchesAdminPage
    searchesReaderPage
    searchesFilteredReaderPage
    rejectsInvalidResults
    rejectsMissingAndPrivateResults
    rejectsStorageErrors
    searchesEveryPhase
    rejectsOversizedPage

check :: String -> Bool -> IO ()
check name condition = unless condition (fail name)

right :: (Show errorValue) => Either errorValue value -> IO value
right = either (fail . show) pure

timestamp :: UTCTime
timestamp = read "2026-01-01 00:00:00 UTC"

draftArticle :: IO Article
draftArticle = do
    identifier <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV")
    content <- right (newDraftContent (const (Right Set.empty))
        (DraftInput "Fixture" "Article body" (Just "haskell-syntax") []))
    Unvalidated <$> right (newUnvalidatedDraft identifier timestamp content)

publishedArticle :: IO Article
publishedArticle = do
    draft <- draftArticle
    case draft of
        Unvalidated article -> do
            available <- right (confirmAvailableImageReferences Set.empty Set.empty)
            proofreaded <- right (proofread timestamp available article)
            excerpt <- right (newExcerpt "Article summary")
            ready <- right (prepareToPublish timestamp excerpt proofreaded)
            Published <$> right (publish timestamp ready)
        _ -> fail "expected unvalidated fixture"

oneRow :: [SQLValue] -> SQLResult
oneRow row = SQLResult [] [row] 0 1

newScript :: [SQLResult] -> IO (ExecuteSQL, IO [SQLStatement])
newScript responses = do
    pending <- newIORef responses
    statements <- newIORef []
    let execute statement = do
            modifyIORef' statements (<> [statement])
            next <- atomicModifyIORef' pending $ \remaining -> case remaining of
                [] -> ([], Nothing)
                value : rest -> (rest, Just value)
            maybe (fail "unexpected query") pure next
    pure (execute, readIORef statements)

readsCatalogSnapshot :: IO ()
readsCatalogSnapshot = do
    (execute, statements) <- newScript [oneRow [SQLNumber 17]]
    snapshot <- readCatalogSnapshotWith execute
    check "catalog snapshot is the append-only outbox count" (snapshot == Right "17")
    issued <- statements
    check "catalog snapshot reads no aggregate payload" (case issued of
        [statement] -> statement.sql == "SELECT COUNT(*) FROM article_outbox"
            && null statement.parameters
        _ -> False)

storedRow :: Article -> IO SQLResult
storedRow article = do
    encoded <- right (articleCodec.encodeArticle article)
    pure (oneRow [SQLText "haskell-syntax", SQLText encoded, SQLNumber 1])

versions :: IO (IORef (VersionContext ArticleIdentifier))
versions = newIORef (emptyVersionContext "Article" articleIdentifierText)

findsSlugOwner :: IO ()
findsSlugOwner = do
    article <- draftArticle
    slug <- right (newSlug "haskell-syntax")
    (execute, statements) <- newScript [oneRow [SQLText (articleIdentifierText (articleIdentifier article))]]
    owner <- findSlugOwnerWith execute slug
    check "slug owner is found" (owner == Right (Just (articleIdentifier article)))
    issued <- statements
    check "slug lookup is indexed" (case issued of
        [statement] -> statement.sql ==
            "SELECT identifier FROM article_aggregates WHERE slug = ? LIMIT 2"
            && statement.parameters == [SQLText "haskell-syntax"]
        _ -> False)
    (missing, _) <- newScript [SQLResult [] [] 0 0]
    absent <- findSlugOwnerWith missing slug
    check "unused slug has no owner" (absent == Right Nothing)

findsArticleBySlug :: IO ()
findsArticleBySlug = do
    article <- draftArticle
    slug <- right (newSlug "haskell-syntax")
    row <- storedRow article
    (execute, _) <- newScript
        [oneRow [SQLText (articleIdentifierText (articleIdentifier article))], row]
    tracked <- versions
    found <- findArticleBySlugWith execute tracked articleCodec slug
    check "slug lookup loads aggregate in the same transaction" (found == Right (Just article))

searchesAdminPage :: IO ()
searchesAdminPage = do
    article <- draftArticle
    criteria <- right (newCriteria UnvalidatedOnly 2 (Just 10))
    row <- storedRow article
    (execute, statements) <- newScript
        [oneRow [SQLNumber 12], oneRow [SQLText (articleIdentifierText (articleIdentifier article))], row]
    tracked <- versions
    result <- searchArticlesWith execute tracked articleCodec criteria
    check "admin page contains count and aggregate" (result == Right (12, [article]))
    issued <- statements
    check "count and page use the same state predicate" (case issued of
        [count, page, _] ->
            count.sql == "SELECT COUNT(*) FROM article_aggregates WHERE phase = ?"
                && count.parameters == [SQLText "unvalidated"]
                && page.sql == "SELECT identifier FROM article_aggregates WHERE phase = ? "
                    <> "ORDER BY updated_order DESC, identifier DESC LIMIT ? OFFSET ?"
                && page.parameters == [SQLText "unvalidated", SQLNumber 10, SQLNumber 10]
        _ -> False)

searchesReaderPage :: IO ()
searchesReaderPage = do
    article <- publishedArticle
    criteria <- right (newCriteria PublishedOnly 1 Nothing)
    row <- storedRow article
    (execute, statements) <- newScript
        [oneRow [SQLNumber 1], oneRow [SQLText (articleIdentifierText (articleIdentifier article))], row]
    tracked <- versions
    result <- searchPublishedArticlesWith execute tracked articleCodec criteria
    check "reader page contains only published articles" (case result of
        Right (1, [found]) -> Published found == article
        _ -> False)
    issued <- statements
    check "reader page uses publication order" (case issued of
        [_, page, _] ->
            "ORDER BY published_order DESC, identifier DESC" `Text.isInfixOf` page.sql
        _ -> False)

searchesFilteredReaderPage :: IO ()
searchesFilteredReaderPage = do
    article <- publishedArticle
    tag <- right (newTagIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAY")
    criteria <- right (newReaderCriteria 1 (Just 10) (Just "ÉCOLE") [tag])
    row <- storedRow article
    (execute, statements) <- newScript
        [oneRow [SQLNumber 1], oneRow [SQLText (articleIdentifierText (articleIdentifier article))], row]
    tracked <- versions
    found <- searchPublishedArticlesWith execute tracked articleCodec criteria
    check "filtered reader query returns published article" (case found of
        Right (1, [value]) -> Published value == article
        _ -> False)
    issued <- statements
    check "count and page apply the same keyword and tag predicates" (case issued of
        [count, page, _] ->
            "json_each(article_aggregates.search_text)" `Text.isInfixOf` count.sql
                && "json_each(article_aggregates.payload, '$.tags')" `Text.isInfixOf` count.sql
                && "json_each(article_aggregates.search_text)" `Text.isInfixOf` page.sql
                && "json_each(article_aggregates.payload, '$.tags')" `Text.isInfixOf` page.sql
                && count.parameters ==
                    [SQLText "published", SQLText "école", SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAY"]
                && take 3 page.parameters == count.parameters
        _ -> False)

rejectsInvalidResults :: IO ()
rejectsInvalidResults = do
    slug <- right (newSlug "haskell-syntax")
    (badOwner, _) <- newScript [oneRow [SQLText "not-an-identifier"]]
    owner <- findSlugOwnerWith badOwner slug
    check "corrupt stored owner is rejected" (isUnexpected owner)
    (badOwnerShape, _) <- newScript [oneRow [SQLNumber 1]]
    malformedOwner <- findSlugOwnerWith badOwnerShape slug
    check "non-text slug owner is rejected" (isUnexpected malformedOwner)
    (duplicateOwners, _) <- newScript [SQLResult []
        [[SQLText "first"], [SQLText "second"]] 0 2]
    ambiguousOwner <- findSlugOwnerWith duplicateOwners slug
    check "duplicate slug owners are rejected" (isUnexpected ambiguousOwner)
    criteria <- right (newCriteria AllArticles 1 Nothing)
    (badCount, _) <- newScript [oneRow [SQLText "one"]]
    tracked <- versions
    count <- searchArticlesWith badCount tracked articleCodec criteria
    check "corrupt count is rejected" (isUnexpected count)
    (notUsed, statements) <- newScript []
    reader <- searchPublishedArticlesWith notUsed tracked articleCodec criteria
    check "reader query rejects an admin filter" (case reader of
        Left (InvariantViolation _) -> True
        _ -> False)
    check "invalid filter does not query storage" . null =<< statements
    (badPage, _) <- newScript [oneRow [SQLNumber 1], oneRow [SQLText "invalid"]]
    malformed <- searchArticlesWith badPage tracked articleCodec criteria
    check "malformed page identifier fails" (isUnexpected malformed)
    (badPageShape, _) <- newScript [oneRow [SQLNumber 1], oneRow [SQLNumber 2]]
    malformedShape <- searchArticlesWith badPageShape tracked articleCodec criteria
    check "non-text page identifier fails" (isUnexpected malformedShape)
    (badCountShape, _) <- newScript [oneRow [SQLNumber 1.5]]
    fractional <- searchArticlesWith badCountShape tracked articleCodec criteria
    check "fractional count fails" (isUnexpected fractional)
    (negativeCount, _) <- newScript [oneRow [SQLNumber (-1)]]
    negative <- searchArticlesWith negativeCount tracked articleCodec criteria
    check "negative count fails" (isUnexpected negative)
    (nanCount, _) <- newScript [oneRow [SQLNumber (0 / 0)]]
    nan <- searchArticlesWith nanCount tracked articleCodec criteria
    check "NaN count fails" (isUnexpected nan)
    (infiniteCount, _) <- newScript [oneRow [SQLNumber (1 / 0)]]
    infinite <- searchArticlesWith infiniteCount tracked articleCodec criteria
    check "infinite count fails" (isUnexpected infinite)
    (largeCount, _) <- newScript [oneRow [SQLNumber 9007199254740992]]
    large <- searchArticlesWith largeCount tracked articleCodec criteria
    check "unsafe count fails" (isUnexpected large)
  where
    isUnexpected (Left (UnexpectedError err)) = not (null (show err))
    isUnexpected _ = False

rejectsMissingAndPrivateResults :: IO ()
rejectsMissingAndPrivateResults = do
    draft <- draftArticle
    slug <- right (newSlug "haskell-syntax")
    tracked <- versions
    (noSlug, _) <- newScript [SQLResult [] [] 0 0]
    absent <- findArticleBySlugWith noSlug tracked articleCodec slug
    check "unknown slug is absent" (absent == Right Nothing)
    (brokenAggregate, _) <- newScript
        [ oneRow [SQLText (articleIdentifierText (articleIdentifier draft))]
        , oneRow [SQLText "haskell-syntax", SQLText "broken payload", SQLNumber 1]
        ]
    corrupt <- findArticleBySlugWith brokenAggregate tracked articleCodec slug
    check "slug lookup rejects a corrupt aggregate" (case corrupt of
        Left (UnexpectedError _) -> True
        _ -> False)
    criteria <- right (newCriteria AllArticles 1 Nothing)
    (missing, _) <- newScript
        [oneRow [SQLNumber 1], oneRow [SQLText (articleIdentifierText (articleIdentifier draft))],
            SQLResult [] [] 0 0]
    absentPage <- searchArticlesWith missing tracked articleCodec criteria
    check "selected article missing is a storage error" (case absentPage of
        Left (UnexpectedError err) -> not (null (show err))
        _ -> False)
    row <- storedRow draft
    publishedOnly <- right (newCriteria PublishedOnly 1 Nothing)
    readerTracked <- versions
    (privateRow, _) <- newScript
        [oneRow [SQLNumber 1], oneRow [SQLText (articleIdentifierText (articleIdentifier draft))], row]
    wrongPhase <- searchPublishedArticlesWith privateRow readerTracked articleCodec publishedOnly
    check "reader selection rejects non-published aggregate" (case wrongPhase of
        Left (UnexpectedError err) -> not (null (show err))
        _ -> False)

rejectsStorageErrors :: IO ()
rejectsStorageErrors = do
    let unavailable _ = throwIO (SQLError "storage unavailable")
    slug <- right (newSlug "haskell-syntax")
    owner <- findSlugOwnerWith unavailable slug
    check "slug storage failure maps to domain error" (case owner of
        Left (ServiceUnavailable err) -> not (null (show err))
        _ -> False)
    criteria <- right (newCriteria AllArticles 1 Nothing)
    tracked <- versions
    searched <- searchArticlesWith unavailable tracked articleCodec criteria
    check "search storage failure maps to domain error" (case searched of
        Left (ServiceUnavailable err) -> not (null (show err))
        _ -> False)
    bySlug <- findArticleBySlugWith unavailable tracked articleCodec slug
    check "slug read failure maps to domain error" (case bySlug of
        Left ServiceUnavailable{} -> True
        _ -> False)
    (selection, _) <- newScript [oneRow [SQLNumber 1]]
    calls <- newIORef (0 :: Int)
    let failSelection statement = do
            step <- atomicModifyIORef' calls $ \value -> (value + 1, value)
            if step == 0 then selection statement else throwIO (SQLError "page unavailable")
    failedPage <- searchArticlesWith failSelection tracked articleCodec criteria
    check "page selection failure maps to domain error" (case failedPage of
        Left ServiceUnavailable{} -> True
        _ -> False)

searchesEveryPhase :: IO ()
searchesEveryPhase = do
    let filters = [(AllArticles, Nothing),
            (ProofreadedOnly, Just "proofreaded"),
            (ReadyOnly, Just "ready"),
            (PublishedOnly, Just "published"),
            (PrivateOnly, Just "private")]
    mapM_ checkFilter filters
  where
    checkFilter (filterValue, phase) = do
        criteria <- right (newCriteria filterValue 1 Nothing)
        (execute, statements) <- newScript [oneRow [SQLNumber 0], SQLResult [] [] 0 0]
        tracked <- versions
        found <- searchArticlesWith execute tracked articleCodec criteria
        check "empty filtered page" (found == Right (0, []))
        issued <- statements
        check "search phase predicate" (case (phase, issued) of
            (Nothing, [count, page]) ->
                count.sql == "SELECT COUNT(*) FROM article_aggregates"
                    && null count.parameters
                    && page.parameters == [SQLNumber 10, SQLNumber 0]
            (Just expected, [count, page]) ->
                count.parameters == [SQLText expected]
                    && head page.parameters == SQLText expected
            _ -> False)

rejectsOversizedPage :: IO ()
rejectsOversizedPage = do
    criteria <- right (newCriteria AllArticles 9007199254740992 Nothing)
    tracked <- versions
    (execute, statements) <- newScript []
    rejected <- searchArticlesWith execute tracked articleCodec criteria
    check "page outside SQL number range is rejected" (case rejected of
        Left err -> not (null (show err))
        _ -> False)
    check "oversized page does not execute SQL" . null =<< statements
