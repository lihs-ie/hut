{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.DurableObject.Repository (
    ArticleCodec (..),
    OutboxRecord (..),
    ExecuteSQL,
    initializeSchema,
    initializeSchemaWith,
    findArticle,
    findArticleWith,
    persistArticle,
    persistArticleWith,
    terminateArticle,
    terminateArticleWith,
    appendOutbox,
    appendOutboxWith,
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
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, diffTimeToPicoseconds, formatTime, utctDay, utctDayTime)
import "article" Domain.Article (Article (..), ArticleIdentifier, articleIdentifier, articleIdentifierText)
import "article" Domain.Article.Draft (draftContent, draftTimeline, proofreadedContent, publicationContent)
import Shared.Domain.Common.Primitive (newPositiveInteger)
import Shared.Domain.Error (
    DomainError,
    createOperationNotAllowed,
    createProcessingTargetChanged,
    createServiceUnavailable,
    createUnexpectedError,
 )
import Shared.Domain.Slug (Slug, slugText)
import Shared.Infrastructure.Versioning (
    PersistenceMode (..),
    Version,
    VersionContext,
    initialVersion,
    newVersion,
    nextVersion,
    observe,
    persistenceMode,
    recordPersisted,
    recordTerminated,
    terminationVersion,
    versionInteger,
 )

data ArticleCodec = ArticleCodec
    { encodeArticle :: Article -> Either DomainError Text
    , decodeArticle :: Text -> Either DomainError Article
    }

-- The caller must use the same storage inside doStorageTransactionWith and
-- create a fresh VersionContext IORef for each transaction invocation.
type ExecuteSQL = SQLStatement -> IO SQLResult

data OutboxRecord = OutboxRecord
    { identifier :: Text
    , article :: ArticleIdentifier
    , eventKind :: Text
    , payload :: Text
    , expectedRevision :: Maybe Version
    }

sqlLimits :: SQLLimits
sqlLimits = SQLLimits{maximumRows = 3, maximumBytes = 16777216, maximumStatements = 1}

storageSQL :: DurableObjectStorage -> ExecuteSQL
storageSQL storage = sqlExec storage sqlLimits

initializeSchema :: DurableObjectStorage -> ArticleCodec -> IO (Either DomainError ())
initializeSchema storage = initializeSchemaWith (storageSQL storage)

initializeSchemaWith :: ExecuteSQL -> ArticleCodec -> IO (Either DomainError ())
initializeSchemaWith execute codec = do
    created <- executeSQL execute articleSchema []
    case created of
        Left err -> pure (Left err)
        Right _ -> do
            outbox <- executeSQL execute outboxSchema []
            case outbox of
                Left err -> pure (Left err)
                Right _ -> do
                    migrated <- migrateArticleSchema execute codec
                    case migrated of
                        Left err -> pure (Left err)
                        Right () -> createIndexes [adminIndex, filteredAdminIndex, readerIndex, outboxIndex]
  where
    articleSchema =
        "CREATE TABLE IF NOT EXISTS article_aggregates ("
            <> "identifier TEXT PRIMARY KEY, "
            <> "slug TEXT UNIQUE, "
            <> "phase TEXT NOT NULL, "
            <> "updated_order TEXT NOT NULL, "
            <> "published_order TEXT, "
            <> "payload TEXT NOT NULL, "
            <> "revision INTEGER NOT NULL CHECK (revision > 0 AND revision <= 9007199254740991))"
    outboxSchema =
        "CREATE TABLE IF NOT EXISTS article_outbox ("
            <> "identifier TEXT PRIMARY KEY, "
            <> "article_identifier TEXT NOT NULL, "
            <> "event_kind TEXT NOT NULL, "
            <> "payload TEXT NOT NULL, "
            <> "expected_revision INTEGER CHECK "
            <> "(expected_revision > 0 AND expected_revision <= 9007199254740991), "
            <> "status TEXT NOT NULL DEFAULT 'pending', "
            <> "attempts INTEGER NOT NULL DEFAULT 0)"
    outboxIndex =
        "CREATE INDEX IF NOT EXISTS article_outbox_pending "
            <> "ON article_outbox (status, identifier)"
    adminIndex =
        "CREATE INDEX IF NOT EXISTS article_admin_order "
            <> "ON article_aggregates (updated_order DESC, identifier DESC)"
    filteredAdminIndex =
        "CREATE INDEX IF NOT EXISTS article_admin_phase_order "
            <> "ON article_aggregates (phase, updated_order DESC, identifier DESC)"
    readerIndex =
        "CREATE INDEX IF NOT EXISTS article_reader_order "
            <> "ON article_aggregates (phase, published_order DESC, identifier DESC)"
    createIndexes [] = pure (Right ())
    createIndexes (statement : rest) = do
        result <- executeSQL execute statement []
        case result of
            Left err -> pure (Left err)
            Right _ -> createIndexes rest

migrateArticleSchema :: ExecuteSQL -> ArticleCodec -> IO (Either DomainError ())
migrateArticleSchema execute codec = do
    columns <- executeSQL execute
        ( "SELECT name FROM pragma_table_info('article_aggregates') "
            <> "WHERE name IN ('phase', 'updated_order', 'published_order')"
        )
        []
    case columns >>= readIndexColumns of
        Left err -> pure (Left err)
        Right present -> do
            added <- addMissing (filter (\(name, _) -> name `notElem` present) missing)
            case added of
                Left err -> pure (Left err)
                Right () -> backfill
  where
    missing =
        [ ("phase", "ALTER TABLE article_aggregates ADD COLUMN phase TEXT NOT NULL DEFAULT ''")
        , ("updated_order", "ALTER TABLE article_aggregates ADD COLUMN updated_order TEXT NOT NULL DEFAULT ''")
        , ("published_order", "ALTER TABLE article_aggregates ADD COLUMN published_order TEXT")
        ]
    addMissing remaining = case remaining of
        [] -> pure (Right ())
        ((_, statement) : rest) -> do
            added <- executeSQL execute statement []
            case added of
                Left err -> pure (Left err)
                Right _ -> addMissing rest
    backfill = do
        found <- executeSQL execute
            ( "SELECT identifier, slug, payload FROM article_aggregates "
                <> "WHERE phase = '' OR updated_order = '' LIMIT 1"
            )
            []
        case found >>= readLegacyArticle codec of
            Left err -> pure (Left err)
            Right Nothing -> pure (Right ())
            Right (Just (identifier, slug, payload, article)) -> do
                updated <- executeSQL execute
                    ( "UPDATE article_aggregates SET phase = ?, updated_order = ?, "
                        <> "published_order = ? WHERE identifier = ? AND slug IS ? "
                        <> "AND payload = ? RETURNING identifier"
                    )
                    (articleIndexParameters article <> [identifier, slug, payload])
                case updated >>= changedExactlyOnce identifier of
                    Left err -> pure (Left err)
                    Right False -> pure (Left staleArticle)
                    Right True -> backfill

readIndexColumns :: SQLResult -> Either DomainError [Text]
readIndexColumns result = traverse readColumn result.rows
  where
    readColumn [SQLText name]
        | name `elem` ["phase", "updated_order", "published_order"] = Right name
    readColumn _ = Left (corruptStorage "article index schema has an invalid row")

readLegacyArticle :: ArticleCodec -> SQLResult -> Either DomainError (Maybe (SQLValue, SQLValue, SQLValue, Article))
readLegacyArticle codec result = case result.rows of
    [] -> Right Nothing
    [[identifier@(SQLText rawIdentifier), slug, payload@(SQLText encoded)]] -> do
        article <- either
            (const (Left (corruptStorage "legacy article payload fails domain validation")))
            Right
            (codec.decodeArticle encoded)
        if articleIdentifierText (articleIdentifier article) /= rawIdentifier
            || slug /= maybe SQLNull (SQLText . slugText) (articleSlug article)
            then Left (corruptStorage "legacy article indexes differ from the payload")
            else Right (Just (identifier, slug, payload, article))
    _ -> Left (corruptStorage "legacy article query returned an invalid row")

findArticle ::
    DurableObjectStorage ->
    IORef (VersionContext ArticleIdentifier) ->
    ArticleCodec ->
    ArticleIdentifier ->
    IO (Either DomainError (Maybe Article))
findArticle storage = findArticleWith (storageSQL storage)

findArticleWith ::
    ExecuteSQL ->
    IORef (VersionContext ArticleIdentifier) ->
    ArticleCodec ->
    ArticleIdentifier ->
    IO (Either DomainError (Maybe Article))
findArticleWith execute versions codec identifier = do
    result <-
        executeSQL
            execute
            "SELECT slug, payload, revision FROM article_aggregates WHERE identifier = ? LIMIT 2"
            [SQLText (articleIdentifierText identifier)]
    case result >>= readArticleRow codec identifier of
        Left err -> pure (Left err)
        Right found -> do
            context <- readIORef versions
            case observe identifier (snd <$> found) context of
                Left err -> pure (Left err)
                Right updated -> do
                    writeIORef versions updated
                    pure (Right (fst <$> found))

persistArticle ::
    DurableObjectStorage ->
    IORef (VersionContext ArticleIdentifier) ->
    ArticleCodec ->
    Article ->
    IO (Either DomainError ())
persistArticle storage = persistArticleWith (storageSQL storage)

persistArticleWith ::
    ExecuteSQL ->
    IORef (VersionContext ArticleIdentifier) ->
    ArticleCodec ->
    Article ->
    IO (Either DomainError ())
persistArticleWith execute versions codec article = do
    context <- readIORef versions
    let identifier = articleIdentifier article
        prepared = do
            mode <- persistenceMode identifier context
            encoded <- codec.encodeArticle article
            let slug = maybe SQLNull (SQLText . slugText) (articleSlug article)
            case mode of
                Insert -> Right (mode, insertStatement identifier slug encoded article, initialVersion)
                Update previous -> do
                    next <- checkedNextVersion previous
                    Right (mode, updateStatement identifier slug encoded article previous next, next)
    case prepared of
        Left err -> pure (Left err)
        Right (mode, statement, next) -> do
            written <- executeSQL execute statement.sql statement.parameters
            case written >>= changedExactlyOnce (versionNumber next) of
                Left err -> pure (Left err)
                Right False -> diagnoseWriteConflict execute identifier mode (articleSlug article)
                Right True -> do
                    writeIORef versions (recordPersisted identifier next context)
                    pure (Right ())

terminateArticle ::
    DurableObjectStorage ->
    IORef (VersionContext ArticleIdentifier) ->
    ArticleIdentifier ->
    IO (Either DomainError ())
terminateArticle storage = terminateArticleWith (storageSQL storage)

terminateArticleWith ::
    ExecuteSQL ->
    IORef (VersionContext ArticleIdentifier) ->
    ArticleIdentifier ->
    IO (Either DomainError ())
terminateArticleWith execute versions identifier = do
    context <- readIORef versions
    case terminationVersion identifier context of
        Left err -> pure (Left err)
        Right expected -> do
            deleted <-
                executeSQL
                    execute
                    ( "DELETE FROM article_aggregates "
                        <> "WHERE identifier = ? AND revision = ? RETURNING identifier"
                    )
                    [SQLText (articleIdentifierText identifier), versionNumber expected]
            case deleted >>= changedExactlyOnce (SQLText (articleIdentifierText identifier)) of
                Left err -> pure (Left err)
                Right False -> pure (Left staleArticle)
                Right True -> do
                    writeIORef versions (recordTerminated identifier context)
                    pure (Right ())

-- Append through the same transaction-local storage after Persist/Terminate.
-- Returning Left must make the transaction driver roll back, not commit.
appendOutbox :: DurableObjectStorage -> OutboxRecord -> IO (Either DomainError ())
appendOutbox storage = appendOutboxWith (storageSQL storage)

appendOutboxWith :: ExecuteSQL -> OutboxRecord -> IO (Either DomainError ())
appendOutboxWith execute record
    | Text.null record.identifier || Text.null record.eventKind =
        pure (Left (createUnexpectedError "Outbox" "identifier and event kind are required"))
    | otherwise = do
        written <-
            executeSQL
                execute
                ( "INSERT INTO article_outbox "
                    <> "(identifier, article_identifier, event_kind, payload, expected_revision) "
                    <> "VALUES (?, ?, ?, ?, ?) "
                    <> "ON CONFLICT(identifier) DO NOTHING RETURNING identifier"
                )
                [ SQLText record.identifier
                , SQLText (articleIdentifierText record.article)
                , SQLText record.eventKind
                , SQLText record.payload
                , maybe SQLNull versionNumber record.expectedRevision
                ]
        pure $ case written >>= changedExactlyOnce (SQLText record.identifier) of
            Left err -> Left err
            Right False -> Left (createOperationNotAllowed "Outbox" "event identifier already exists")
            Right True -> Right ()

executeSQL :: ExecuteSQL -> Text -> [SQLValue] -> IO (Either DomainError SQLResult)
executeSQL execute statement parameters = do
    outcome <- try (execute SQLStatement{sql = statement, parameters})
    pure $ case outcome of
        Left (errorValue :: SQLError) ->
            Left (createServiceUnavailable "ArticleStorage" (Text.pack (show errorValue)))
        Right result -> Right result

readArticleRow ::
    ArticleCodec ->
    ArticleIdentifier ->
    SQLResult ->
    Either DomainError (Maybe (Article, Version))
readArticleRow codec identifier result = case result.rows of
    [] -> Right Nothing
    [[rawSlug, SQLText encoded, rawVersion]] -> do
        version <- decodeVersion rawVersion
        article <-
            either
                (const (Left (corruptStorage "article payload fails domain validation")))
                Right
                (codec.decodeArticle encoded)
        if articleIdentifier article /= identifier
            then Left (corruptStorage "decoded article identifier differs from the stored key")
            else
                if rawSlug /= maybe SQLNull (SQLText . slugText) (articleSlug article)
                    then Left (corruptStorage "decoded article slug differs from the stored index")
                    else Right (Just (article, version))
    _ -> Left (corruptStorage "article query returned an invalid row shape")

decodeVersion :: SQLValue -> Either DomainError Version
decodeVersion (SQLNumber number)
    | not (isNaN number || isInfinite number)
        && number >= 1
        && number <= fromInteger maxSafeInteger
        && fromInteger (round number) == number =
        newVersion <$> newPositiveInteger (round number)
decodeVersion _ = Left (corruptStorage "revision is not a safe positive integer")

checkedNextVersion :: Version -> Either DomainError Version
checkedNextVersion previous
    | versionInteger previous < maxSafeInteger = Right (nextVersion previous)
    | otherwise = Left (createOperationNotAllowed "Article" "storage revision limit reached")

versionNumber :: Version -> SQLValue
versionNumber = SQLNumber . fromInteger . versionInteger

maxSafeInteger :: Integer
maxSafeInteger = 9007199254740991

insertStatement :: ArticleIdentifier -> SQLValue -> Text -> Article -> SQLStatement
insertStatement identifier slug encoded article =
    SQLStatement
        { sql =
            "INSERT INTO article_aggregates "
                <> "(identifier, slug, phase, updated_order, published_order, payload, revision) "
                <> "VALUES (?, ?, ?, ?, ?, ?, 1) "
                <> "ON CONFLICT DO NOTHING RETURNING revision"
        , parameters =
            [SQLText (articleIdentifierText identifier), slug]
                <> articleIndexParameters article
                <> [SQLText encoded]
        }

updateStatement :: ArticleIdentifier -> SQLValue -> Text -> Article -> Version -> Version -> SQLStatement
updateStatement identifier slug encoded article previous next =
    SQLStatement
        { sql =
            "UPDATE OR IGNORE article_aggregates "
                <> "SET slug = ?, phase = ?, updated_order = ?, published_order = ?, "
                <> "payload = ?, revision = ? "
                <> "WHERE identifier = ? AND revision = ? RETURNING revision"
        , parameters =
            [slug]
                <> articleIndexParameters article
                <> [ SQLText encoded
            , versionNumber next
            , SQLText (articleIdentifierText identifier)
            , versionNumber previous
                ]
        }

articleIndexParameters :: Article -> [SQLValue]
articleIndexParameters article =
    [ SQLText phase
    , SQLText (timeOrder updatedAt)
    , maybe SQLNull (SQLText . timeOrder) publishedAt
    ]
  where
    (phase, updatedAt, publishedAt) = case article of
        Unvalidated draft -> ("unvalidated", (draftTimeline draft).updatedAt, Nothing)
        Proofreaded draft -> ("proofreaded", (draftTimeline draft).updatedAt, Nothing)
        Ready draft -> ("ready", (draftTimeline draft).updatedAt, Nothing)
        Published value -> ("published", value.timeline.updatedAt, Just value.publishedAt)
        Private value -> ("private", value.timeline.updatedAt, Just value.publishedAt)

timeOrder :: UTCTime -> Text
timeOrder value =
    Text.pack (formatTime defaultTimeLocale "%Y%m%d" (utctDay value))
        <> Text.justifyRight 17 '0'
            (Text.pack (show (diffTimeToPicoseconds (utctDayTime value))))

changedExactlyOnce :: SQLValue -> SQLResult -> Either DomainError Bool
changedExactlyOnce expected result = case result.rows of
    [] -> Right False
    [[actual]] | actual == expected -> Right True
    _ -> Left (corruptStorage "write returned an unexpected row")

diagnoseWriteConflict ::
    ExecuteSQL -> ArticleIdentifier -> PersistenceMode -> Maybe Slug -> IO (Either DomainError ())
diagnoseWriteConflict execute identifier mode slug = do
    current <-
        executeSQL
            execute
            "SELECT revision FROM article_aggregates WHERE identifier = ? LIMIT 2"
            [SQLText (articleIdentifierText identifier)]
    case current >>= readCurrentVersion of
        Left err -> pure (Left err)
        Right found -> case mode of
            Insert | found /= Nothing ->
                pure (Left (createOperationNotAllowed "Article" "identifier already exists"))
            Update expected | found /= Just expected -> pure (Left staleArticle)
            _ -> checkSlugConflict execute identifier slug

checkSlugConflict ::
    ExecuteSQL -> ArticleIdentifier -> Maybe Slug -> IO (Either DomainError ())
checkSlugConflict execute identifier slug = do
    owner <- case slug of
        Nothing -> pure (Right Nothing)
        Just value -> do
            result <-
                executeSQL
                    execute
                    "SELECT identifier FROM article_aggregates WHERE slug = ? LIMIT 2"
                    [SQLText (slugText value)]
            pure (result >>= readOwner)
    pure $ case owner of
        Left err -> Left err
        Right (Just other) | other /= articleIdentifierText identifier ->
            Left (createOperationNotAllowed "Slug" "slug already belongs to another article")
        Right _ -> Left staleArticle

readCurrentVersion :: SQLResult -> Either DomainError (Maybe Version)
readCurrentVersion result = case result.rows of
    [] -> Right Nothing
    [[rawVersion]] -> Just <$> decodeVersion rawVersion
    _ -> Left (corruptStorage "revision lookup returned an invalid row shape")

readOwner :: SQLResult -> Either DomainError (Maybe Text)
readOwner result = case result.rows of
    [] -> Right Nothing
    [[SQLText identifier]] -> Right (Just identifier)
    _ -> Left (corruptStorage "slug lookup returned an invalid row shape")

articleSlug :: Article -> Maybe Slug
articleSlug (Unvalidated draft) = (draftContent draft).slug
articleSlug (Proofreaded draft) = Just (proofreadedContent draft).slug
articleSlug (Ready draft) = Just (publicationContent draft).slug
articleSlug (Published article) = Just article.publication.slug
articleSlug (Private article) = Just article.publication.slug

staleArticle :: DomainError
staleArticle = createProcessingTargetChanged "Article" "the observed article changed"

corruptStorage :: Text -> DomainError
corruptStorage = createUnexpectedError "ArticleStorage"
