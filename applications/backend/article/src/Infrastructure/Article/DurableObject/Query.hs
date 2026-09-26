{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.DurableObject.Query (
    findArticleBySlug,
    findArticleBySlugWith,
    findSlugOwner,
    findSlugOwnerWith,
    searchArticles,
    searchArticlesWith,
    searchPublishedArticles,
    searchPublishedArticlesWith,
    readCatalogSnapshot,
    readCatalogSnapshotWith,
) where

import Cloudflare.Workers.Binding.DurableObject (DurableObjectStorage)
import Cloudflare.Workers.Binding.DurableObject.SQL (
    SQLError,
    SQLLimits (..),
    SQLResult (..),
    SQLStatement (..),
    SQLValue (..),
    sqlExec,
 )
import Control.Exception (try)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Text qualified as Text
import "article" Domain.Article (
    Article (..),
    ArticleIdentifier,
    newArticleIdentifier,
 )
import "article" Domain.Article.Criteria (
    ArticleFilter (..),
    Criteria,
    keyword,
    pageOffset,
    pageSize,
    status,
    tags,
 )
import "article" Domain.Article.Published (PublishedArticle)
import Infrastructure.Article.DurableObject.Repository (
    ArticleCodec,
    ExecuteSQL,
    findArticleWith,
 )
import "shared" Shared.Domain.Error (
    DomainError,
    createInvariantViolation,
    createServiceUnavailable,
    createUnexpectedError,
 )
import "shared" Shared.Domain.Slug (Slug, slugText)
import "shared" Shared.Domain.Tag (tagIdentifierText)
import "shared" Shared.Infrastructure.Versioning (VersionContext)

queryLimits :: SQLLimits
queryLimits = SQLLimits{maximumRows = 100, maximumBytes = 16777216, maximumStatements = 1}

storageSQL :: DurableObjectStorage -> ExecuteSQL
storageSQL storage = sqlExec storage queryLimits

-- Outbox rows are append-only; every published-list mutation appends an event.
readCatalogSnapshot :: DurableObjectStorage -> IO (Either DomainError Text)
readCatalogSnapshot storage = readCatalogSnapshotWith (storageSQL storage)

readCatalogSnapshotWith :: ExecuteSQL -> IO (Either DomainError Text)
readCatalogSnapshotWith execute = do
    counted <- executeSQL execute "SELECT COUNT(*) FROM article_outbox" []
    pure (Text.pack . show <$> (counted >>= readCount))

findArticleBySlug ::
    DurableObjectStorage ->
    IORef (VersionContext ArticleIdentifier) ->
    ArticleCodec ->
    Slug ->
    IO (Either DomainError (Maybe Article))
findArticleBySlug storage = findArticleBySlugWith (storageSQL storage)

findArticleBySlugWith ::
    ExecuteSQL ->
    IORef (VersionContext ArticleIdentifier) ->
    ArticleCodec ->
    Slug ->
    IO (Either DomainError (Maybe Article))
findArticleBySlugWith execute versions codec slug = do
    owner <- findSlugOwnerWith execute slug
    case owner of
        Left err -> pure (Left err)
        Right Nothing -> pure (Right Nothing)
        Right (Just identifier) -> findArticleWith execute versions codec identifier

findSlugOwner :: DurableObjectStorage -> Slug -> IO (Either DomainError (Maybe ArticleIdentifier))
findSlugOwner storage = findSlugOwnerWith (storageSQL storage)

findSlugOwnerWith :: ExecuteSQL -> Slug -> IO (Either DomainError (Maybe ArticleIdentifier))
findSlugOwnerWith execute slug = do
    selected <- executeSQL execute
        "SELECT identifier FROM article_aggregates WHERE slug = ? LIMIT 2"
        [SQLText (slugText slug)]
    pure $ selected >>= \result -> case result.rows of
        [] -> Right Nothing
        [[SQLText raw]] -> Just <$> storedIdentifier raw
        _ -> Left (corruptQuery "slug lookup returned an invalid row")

searchArticles ::
    DurableObjectStorage ->
    IORef (VersionContext ArticleIdentifier) ->
    ArticleCodec ->
    Criteria ->
    IO (Either DomainError (Int, [Article]))
searchArticles storage = searchArticlesWith (storageSQL storage)

searchArticlesWith ::
    ExecuteSQL ->
    IORef (VersionContext ArticleIdentifier) ->
    ArticleCodec ->
    Criteria ->
    IO (Either DomainError (Int, [Article]))
searchArticlesWith execute versions codec criteria =
    searchWith execute versions codec criteria "updated_order" (filterFor criteria)

searchPublishedArticles ::
    DurableObjectStorage ->
    IORef (VersionContext ArticleIdentifier) ->
    ArticleCodec ->
    Criteria ->
    IO (Either DomainError (Int, [PublishedArticle]))
searchPublishedArticles storage = searchPublishedArticlesWith (storageSQL storage)

searchPublishedArticlesWith ::
    ExecuteSQL ->
    IORef (VersionContext ArticleIdentifier) ->
    ArticleCodec ->
    Criteria ->
    IO (Either DomainError (Int, [PublishedArticle]))
searchPublishedArticlesWith execute versions codec criteria
    | status criteria /= PublishedOnly =
        pure (Left (createInvariantViolation "ArticleSearch" "published selection is required"))
    | otherwise = do
        found <- searchWith execute versions codec criteria "published_order"
            (readerFilterFor criteria)
        pure $ found >>= \(total, articles) -> do
            published <- traverse requirePublished articles
            pure (total, published)
  where
    requirePublished (Published article) = Right article
    requirePublished _ = Left (corruptQuery "reader page contains a non-published article")

searchWith ::
    ExecuteSQL ->
    IORef (VersionContext ArticleIdentifier) ->
    ArticleCodec ->
    Criteria ->
    Text ->
    (Text, [SQLValue]) ->
    IO (Either DomainError (Int, [Article]))
searchWith execute versions codec criteria orderColumn (whereClause, filterParameters) =
    case pageBounds criteria of
        Left err -> pure (Left err)
        Right (limit, offset) -> do
            counted <- executeSQL execute
                ("SELECT COUNT(*) FROM article_aggregates" <> whereClause)
                filterParameters
            case counted >>= readCount of
                Left err -> pure (Left err)
                Right total -> do
                    selected <- executeSQL execute
                        ( "SELECT identifier FROM article_aggregates" <> whereClause
                            <> " ORDER BY " <> orderColumn <> " DESC, identifier DESC LIMIT ? OFFSET ?"
                        )
                        (filterParameters <> [SQLNumber limit, SQLNumber offset])
                    case selected >>= traverse readIdentifierRow . (.rows) of
                        Left err -> pure (Left err)
                        Right identifiers -> do
                            loaded <- traverse (findArticleWith execute versions codec) identifiers
                            pure $ do
                                articles <- sequence loaded >>= traverse requireArticle
                                pure (total, articles)
  where
    requireArticle = maybe (Left (corruptQuery "selected article disappeared")) Right

filterFor :: Criteria -> (Text, [SQLValue])
filterFor criteria = case status criteria of
    AllArticles -> ("", [])
    UnvalidatedOnly -> selected "unvalidated"
    ProofreadedOnly -> selected "proofreaded"
    ReadyOnly -> selected "ready"
    PublishedOnly -> selected "published"
    PrivateOnly -> selected "private"
  where
    selected phase = (" WHERE phase = ?", [SQLText phase])

readerFilterFor :: Criteria -> (Text, [SQLValue])
readerFilterFor criteria =
    (" WHERE phase = ?" <> keywordClause <> tagClause,
        [SQLText "published"] <> keywordParameters <> tagParameters)
  where
    (keywordClause, keywordParameters) = case keyword criteria of
        Nothing -> ("", [])
        Just value ->
            ( " AND (instr(lower(json_extract(payload, '$.title')), lower(?)) > 0"
                <> " OR instr(lower(json_extract(payload, '$.body')), lower(?)) > 0"
                <> " OR instr(lower(json_extract(payload, '$.excerpt')), lower(?)) > 0)"
            , replicate 3 (SQLText value)
            )
    selectedTags = map (SQLText . tagIdentifierText) (tags criteria)
    (tagClause, tagParameters)
        | null selectedTags = ("", [])
        | otherwise =
            ( " AND EXISTS (SELECT 1 FROM json_each(article_aggregates.payload, '$.tags') AS tag"
                <> " WHERE tag.value IN ("
                <> Text.intercalate ", " (replicate (length selectedTags) "?")
                <> "))"
            , selectedTags
            )

pageBounds :: Criteria -> Either DomainError (Double, Double)
pageBounds criteria =
    (,) <$> safeNumber (pageSize criteria) <*> safeNumber (pageOffset criteria)
  where
    safeNumber number
        | number < 0 || toInteger number > 9007199254740991 =
            Left (createInvariantViolation "Pagination" "page exceeds storage number range")
        | otherwise = Right (fromIntegral number)

readCount :: SQLResult -> Either DomainError Int
readCount result = case result.rows of
    [[SQLNumber number]]
        | not (isNaN number || isInfinite number)
            && number >= 0
            && number <= 9007199254740991
            && fromIntegral (round number :: Integer) == number ->
            Right (round number)
    _ -> Left (corruptQuery "count returned an invalid row")

readIdentifierRow :: [SQLValue] -> Either DomainError ArticleIdentifier
readIdentifierRow [SQLText raw] = storedIdentifier raw
readIdentifierRow _ = Left (corruptQuery "page returned an invalid identifier")

storedIdentifier :: Text -> Either DomainError ArticleIdentifier
storedIdentifier = either (const (Left (corruptQuery "stored identifier is invalid"))) Right
    . newArticleIdentifier

executeSQL :: ExecuteSQL -> Text -> [SQLValue] -> IO (Either DomainError SQLResult)
executeSQL execute statement parameters = do
    outcome <- try (execute SQLStatement{sql = statement, parameters})
    pure $ case outcome of
        Left (failure :: SQLError) ->
            Left (createServiceUnavailable "ArticleSearch" (Text.pack (show failure)))
        Right result -> Right result

corruptQuery :: Text -> DomainError
corruptQuery = createUnexpectedError "ArticleSearch"
