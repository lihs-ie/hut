{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.DurableObject.RepositorySpec (run) where

import Cloudflare.Workers.Binding.DurableObject.SQL (
    SQLError (..),
    SQLResult (..),
    SQLStatement (..),
    SQLValue (..),
 )
import Control.Monad (unless)
import Control.Exception (throwIO)
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
import "article" Domain.Article.Draft (
    newUnvalidatedDraft,
    prepareToPublish,
    proofread,
 )
import "article" Domain.Article.Private (takeDown)
import "article" Domain.Article.Published (publish)
import Infrastructure.Article.DurableObject.Repository
import Shared.Domain.Excerpt (newExcerpt)
import Shared.Domain.Error (
    DomainError (..),
    createOperationNotAllowed,
    createProcessingTargetChanged,
    createServiceUnavailable,
    createUnexpectedError,
 )
import Shared.Infrastructure.Versioning (
    PersistenceMode (..),
    VersionContext,
    emptyVersionContext,
    initialVersion,
    nextVersion,
    persistenceMode,
 )

check :: String -> Bool -> IO ()
check name condition = unless condition (fail name)

right :: (Show errorValue) => Either errorValue value -> IO value
right = either (fail . show) pure

timestamp :: UTCTime
timestamp = read "2026-01-01 00:00:00 UTC"

start :: IO Article
start = do
    identifier <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV")
    content <-
        right
            (newDraftContent (const (Right Set.empty)) (DraftInput "Fixture" "" (Just "haskell-syntax") []))
    Unvalidated <$> right (newUnvalidatedDraft identifier timestamp content)

firstStatement :: [SQLStatement] -> IO SQLStatement
firstStatement (value : _) = pure value
firstStatement [] = fail "expected first SQL statement"

secondStatement :: [SQLStatement] -> IO SQLStatement
secondStatement (_ : value : _) = pure value
secondStatement _ = fail "expected second SQL statement"

lastStatement :: [SQLStatement] -> IO SQLStatement
lastStatement = firstStatement . reverse

run :: IO ()
run = do
    createsSchema
    migratesLegacySchema
    rejectsMigrationFailures
    rejectsBrokenLegacySchema
    observesMissingArticle
    insertsAndTracksVersion
    findsAndUpdates
    terminatesLoadedArticle
    rejectsStaleWrite
    rejectsOccupiedSlug
    rejectsDuplicateIdentifier
    rejectsCorruptRow
    rejectsStoredSlugMismatch
    rejectsWrongWriteEcho
    appendsOutbox
    rejectsMalformedRows
    rejectsCodecMismatch
    rejectsMissingObservation
    rejectsStaleDelete
    rejectsUnchangedConflict
    rejectsMalformedOutbox
    rejectsRevisionOverflow
    rejectsSchemaFailure
    rejectsChangedObservation
    rejectsEncodingFailure
    rejectsMalformedConflictLookups
    persistsSluglessDraft
    rejectsOutboxSQLFailure
    rejectsSQLFailures
    rejectsMalformedWriteReturns
    diagnosesUpdateConflicts
    diagnosesSlugConflicts
    observesRepeatedReads
    rejectsAdditionalMalformedRows
    validatesOutboxInputs
    persistsLifecycleSlugs

emptyResult :: SQLResult
emptyResult = SQLResult [] [] 0 0

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
            maybe (fail "unexpected SQL statement") pure next
    pure (execute, readIORef statements)

newFailingScript :: Int -> [SQLResult] -> IO (ExecuteSQL, IO [SQLStatement])
newFailingScript failingStep responses = do
    (execute, statements) <- newScript responses
    count <- newIORef (0 :: Int)
    let failAtStep statement = do
            step <- atomicModifyIORef' count $ \value -> (value + 1, value + 1)
            if step == failingStep
                then throwIO (SQLError "storage unavailable")
                else execute statement
    pure (failAtStep, statements)

checkError :: String -> DomainError -> Either DomainError value -> IO ()
checkError name expected result =
    check name (either (== expected) (const False) result)

storedRow :: Article -> Double -> SQLResult
storedRow article revision =
    oneRow
        [ SQLText "haskell-syntax"
        , SQLText (articleIdentifierText (articleIdentifier article))
        , SQLNumber revision
        ]

newVersions :: IO (IORef (VersionContext ArticleIdentifier))
newVersions = newIORef (emptyVersionContext "Article" articleIdentifierText)

-- This narrow fixture codec only exercises SQL/version behavior. The production
-- codec still needs the domain rehydration functions documented in Repository.
fixtureCodec :: ArticleCodec
fixtureCodec =
    ArticleCodec
        { encodeArticle = Right . articleIdentifierText . articleIdentifier
        , decodeArticle = \raw -> do
            identifier <- newArticleIdentifier raw
            content <-
                newDraftContent
                    (const (Right Set.empty))
                    (DraftInput "Fixture" "" (Just "haskell-syntax") [])
            Unvalidated <$> newUnvalidatedDraft identifier timestamp content
        }

createsSchema :: IO ()
createsSchema = do
    (execute, statements) <- newScript
        ([emptyResult, emptyResult, SQLResult [] [[SQLText "phase"], [SQLText "updated_order"], [SQLText "published_order"]] 0 3, emptyResult]
            <> replicate 4 emptyResult)
    result <- initializeSchemaWith execute fixtureCodec
    check "schema initializes" (result == Right ())
    issued <- statements
    articleStatement <- firstStatement issued
    outboxStatement <- secondStatement issued
    check
        "article table created"
        ("CREATE TABLE IF NOT EXISTS article_aggregates" `Text.isInfixOf` articleStatement.sql)
    check "slug unique constraint" ("slug TEXT UNIQUE" `Text.isInfixOf` articleStatement.sql)
    check
        "outbox table created"
        ("CREATE TABLE IF NOT EXISTS article_outbox" `Text.isInfixOf` outboxStatement.sql)
    check "article and outbox indexes created" (length issued == 8)
    indexStatement <- lastStatement issued
    check "outbox pending index targets status and identifier"
        (indexStatement.sql == "CREATE INDEX IF NOT EXISTS article_outbox_pending "
            <> "ON article_outbox (status, identifier)")
    check "schema statements have no parameters"
        (all (\statement -> null statement.parameters) issued)
    check "article phase and sort keys are stored"
        (all (`Text.isInfixOf` articleStatement.sql)
            ["phase TEXT NOT NULL", "updated_order TEXT NOT NULL", "published_order TEXT"])

migratesLegacySchema :: IO ()
migratesLegacySchema = do
    article <- start
    let identifier = articleIdentifierText (articleIdentifier article)
        row = oneRow [SQLText identifier, SQLText "haskell-syntax", SQLText identifier]
        existingColumns = oneRow [SQLText "phase"]
    (execute, statements) <- newScript
        ( [emptyResult, emptyResult, existingColumns, emptyResult, emptyResult, row]
            <> [oneRow [SQLText identifier], emptyResult]
            <> replicate 4 emptyResult
        )
    result <- initializeSchemaWith execute fixtureCodec
    check "legacy schema migrates" (result == Right ())
    issued <- statements
    check "only missing columns are added"
        (length (filter (Text.isPrefixOf "ALTER TABLE" . (.sql)) issued) == 2)
    check "legacy aggregate is backfilled"
        (any (Text.isInfixOf "UPDATE article_aggregates SET phase" . (.sql)) issued)
    check "indexes are created after backfill"
        (Text.isPrefixOf "CREATE INDEX" (last issued).sql)

rejectsBrokenLegacySchema :: IO ()
rejectsBrokenLegacySchema = do
    article <- start
    let identifier = articleIdentifierText (articleIdentifier article)
        columns = SQLResult []
            [[SQLText "phase"], [SQLText "updated_order"], [SQLText "published_order"]]
            0 3
        schemaPrefix = [emptyResult, emptyResult, columns]
        validRow = oneRow
            [SQLText identifier, SQLText "haskell-syntax", SQLText identifier]
        runMigration rows = do
            (execute, _) <- newScript (schemaPrefix <> rows)
            initializeSchemaWith execute fixtureCodec
    malformedColumns <- runMigration [oneRow [SQLNumber 1]]
    check "malformed legacy row is rejected" (isUnexpected malformedColumns)
    malformedPayload <- runMigration
        [oneRow [SQLText identifier, SQLText "haskell-syntax", SQLText "invalid"]]
    check "invalid legacy payload is rejected" (isUnexpected malformedPayload)
    wrongSlug <- runMigration
        [oneRow [SQLText identifier, SQLText "other-slug", SQLText identifier]]
    check "legacy slug mismatch is rejected" (isUnexpected wrongSlug)
    stale <- runMigration [validRow, emptyResult]
    checkError "legacy backfill conflict is reported" staleArticleError stale
    (execute, _) <- newScript
        [emptyResult, emptyResult, oneRow [SQLNumber 1]]
    invalidColumns <- initializeSchemaWith execute fixtureCodec
    check "malformed schema metadata is rejected" (isUnexpected invalidColumns)
  where
    staleArticleError = createProcessingTargetChanged
        "Article" "the observed article changed"

rejectsMigrationFailures :: IO ()
rejectsMigrationFailures = do
    let allColumns = SQLResult []
            [[SQLText "phase"], [SQLText "updated_order"], [SQLText "published_order"]]
            0 3
        runFailure failAt columns = do
            calls <- newIORef (0 :: Int)
            let execute _ = do
                    step <- atomicModifyIORef' calls $ \value -> (value + 1, value + 1)
                    if step == failAt
                        then throwIO (SQLError "migration unavailable")
                        else pure (if step == 3 then columns else emptyResult)
            result <- initializeSchemaWith execute fixtureCodec
            check "migration SQL failure is reported" (isServiceUnavailable result)
            check "schema stops at the failing statement" =<< (== failAt) <$> readIORef calls
    runFailure 3 allColumns
    runFailure 4 emptyResult
    runFailure 4 allColumns
    runFailure 5 allColumns
    article <- start
    let identifier = articleIdentifierText (articleIdentifier article)
        legacy = oneRow [SQLText identifier, SQLText "haskell-syntax", SQLText identifier]
    calls <- newIORef (0 :: Int)
    let failUpdate _ = do
            step <- atomicModifyIORef' calls $ \value -> (value + 1, value + 1)
            if step == 5
                then throwIO (SQLError "backfill unavailable")
                else pure $ case step of
                    3 -> allColumns
                    4 -> legacy
                    _ -> emptyResult
    update <- initializeSchemaWith failUpdate fixtureCodec
    check "legacy backfill SQL failure is reported" (isServiceUnavailable update)

observesMissingArticle :: IO ()
observesMissingArticle = do
    article <- start
    versions <- newVersions
    (execute, _) <- newScript [emptyResult]
    found <- findArticleWith execute versions fixtureCodec (articleIdentifier article)
    check "missing article is returned as Nothing" (found == Right Nothing)
    context <- readIORef versions
    check
        "missing observation allows insert"
        (persistenceMode (articleIdentifier article) context == Right Insert)

insertsAndTracksVersion :: IO ()
insertsAndTracksVersion = do
    article <- start
    versions <- newVersions
    (execute, statements) <- newScript [oneRow [SQLNumber 1]]
    result <- persistArticleWith execute versions fixtureCodec article
    check "insert succeeds" (result == Right ())
    context <- readIORef versions
    check
        "insert advances context to initial revision"
        (persistenceMode (articleIdentifier article) context == Right (Update initialVersion))
    issued <- statements
    statement <- firstStatement issued
    check "insert is conditional" ("ON CONFLICT DO NOTHING" `Text.isInfixOf` statement.sql)
    check "slug is persisted separately" (SQLText "haskell-syntax" `elem` statement.parameters)
    check "insert stores the encoded article"
        (statement.parameters ==
            [ SQLText (articleIdentifierText (articleIdentifier article))
            , SQLText "haskell-syntax"
            , SQLText "unvalidated"
            , SQLText "2026010100000000000000000"
            , SQLNull
            , SQLText (articleIdentifierText (articleIdentifier article))
            ])

findsAndUpdates :: IO ()
findsAndUpdates = do
    article <- start
    versions <- newVersions
    let identifier = articleIdentifier article
    (execute, statements) <-
        newScript
            [ oneRow [SQLText "haskell-syntax", SQLText (articleIdentifierText identifier), SQLNumber 1]
            , oneRow [SQLNumber 2]
            ]
    found <- findArticleWith execute versions fixtureCodec identifier
    check "find restores matching identifier" (fmap (fmap articleIdentifier) found == Right (Just identifier))
    updated <- persistArticleWith execute versions fixtureCodec article
    check "update succeeds" (updated == Right ())
    context <- readIORef versions
    check
        "update advances tracked revision"
        (persistenceMode identifier context == Right (Update (nextVersion initialVersion)))
    issued <- statements
    select <- firstStatement issued
    check "find selects one aggregate by identifier"
        (select.sql == "SELECT slug, payload, revision FROM article_aggregates "
            <> "WHERE identifier = ? LIMIT 2"
            && select.parameters == [SQLText (articleIdentifierText identifier)])
    statement <- lastStatement issued
    check "update is conditional on the observed revision"
        (statement.sql == "UPDATE OR IGNORE article_aggregates "
            <> "SET slug = ?, phase = ?, updated_order = ?, published_order = ?, "
            <> "payload = ?, revision = ? "
            <> "WHERE identifier = ? AND revision = ? RETURNING revision")
    check
        "update compares original revision"
        ( statement.parameters
            == [ SQLText "haskell-syntax"
               , SQLText "unvalidated"
               , SQLText "2026010100000000000000000"
               , SQLNull
               , SQLText (articleIdentifierText identifier)
               , SQLNumber 2
               , SQLText (articleIdentifierText identifier)
               , SQLNumber 1
               ]
        )

terminatesLoadedArticle :: IO ()
terminatesLoadedArticle = do
    article <- start
    versions <- newVersions
    let identifier = articleIdentifier article
    (execute, statements) <-
        newScript
            [ oneRow [SQLText "haskell-syntax", SQLText (articleIdentifierText identifier), SQLNumber 1]
            , oneRow [SQLText (articleIdentifierText identifier)]
            ]
    _ <- findArticleWith execute versions fixtureCodec identifier
    deleted <- terminateArticleWith execute versions identifier
    check "delete succeeds" (deleted == Right ())
    issued <- statements
    statement <- lastStatement issued
    check "delete is conditional on the observed revision"
        (statement.sql == "DELETE FROM article_aggregates "
            <> "WHERE identifier = ? AND revision = ? RETURNING identifier")
    check
        "delete compares revision"
        (statement.parameters == [SQLText (articleIdentifierText identifier), SQLNumber 1])
    context <- readIORef versions
    check
        "deleted aggregate cannot be persisted again"
        (isOperationNotAllowed (persistenceMode identifier context))

rejectsStaleWrite :: IO ()
rejectsStaleWrite = do
    article <- start
    versions <- newVersions
    let identifier = articleIdentifier article
    (execute, _) <-
        newScript
            [ oneRow [SQLText "haskell-syntax", SQLText (articleIdentifierText identifier), SQLNumber 1]
            , emptyResult
            , oneRow [SQLNumber 2]
            ]
    _ <- findArticleWith execute versions fixtureCodec identifier
    result <- persistArticleWith execute versions fixtureCodec article
    check "stale update is rejected" (isProcessingTargetChanged result)
    context <- readIORef versions
    check
        "failed update does not advance revision"
        (persistenceMode identifier context == Right (Update initialVersion))

rejectsOccupiedSlug :: IO ()
rejectsOccupiedSlug = do
    article <- start
    versions <- newVersions
    (execute, _) <-
        newScript [emptyResult, emptyResult, oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]]
    result <- persistArticleWith execute versions fixtureCodec article
    check "slug conflict is rejected" (isOperationNotAllowed result)
    context <- readIORef versions
    check
        "failed insert does not advance revision"
        (persistenceMode (articleIdentifier article) context == Right Insert)

rejectsDuplicateIdentifier :: IO ()
rejectsDuplicateIdentifier = do
    article <- start
    versions <- newVersions
    (execute, _) <- newScript [emptyResult, oneRow [SQLNumber 1]]
    result <- persistArticleWith execute versions fixtureCodec article
    check "duplicate identifier is rejected without overwriting" (isOperationNotAllowed result)

rejectsCorruptRow :: IO ()
rejectsCorruptRow = do
    article <- start
    versions <- newVersions
    (execute, _) <-
        newScript
            [ oneRow
                [ SQLText "haskell-syntax"
                , SQLText (articleIdentifierText (articleIdentifier article))
                , SQLNumber 0
                ]
            ]
    result <- findArticleWith execute versions fixtureCodec (articleIdentifier article)
    check "invalid stored revision is rejected" (case result of Left (UnexpectedError _) -> True; _ -> False)

rejectsStoredSlugMismatch :: IO ()
rejectsStoredSlugMismatch = do
    article <- start
    versions <- newVersions
    let identifier = articleIdentifier article
    (execute, _) <-
        newScript [oneRow [SQLText "other-slug", SQLText (articleIdentifierText identifier), SQLNumber 1]]
    result <- findArticleWith execute versions fixtureCodec identifier
    check "stored slug mismatch is rejected" (isUnexpected result)

rejectsWrongWriteEcho :: IO ()
rejectsWrongWriteEcho = do
    article <- start
    versions <- newVersions
    (execute, _) <- newScript [oneRow [SQLNumber 2]]
    result <- persistArticleWith execute versions fixtureCodec article
    check "unexpected inserted revision is rejected" (isUnexpected result)

appendsOutbox :: IO ()
appendsOutbox = do
    article <- start
    let record =
            OutboxRecord
                { identifier = "event-1"
                , article = articleIdentifier article
                , eventKind = "ArticleProofreaded"
                , payload = "{}"
                , expectedRevision = Just initialVersion
                }
    (execute, statements) <- newScript [oneRow [SQLText "event-1"], emptyResult]
    inserted <- appendOutboxWith execute record
    duplicate <- appendOutboxWith execute record
    check "outbox insert succeeds" (inserted == Right ())
    check "duplicate outbox identifier is rejected" (isOperationNotAllowed duplicate)
    issued <- statements
    statement <- firstStatement issued
    check "outbox stores expected revision" (SQLNumber 1 `elem` statement.parameters)
    check "outbox insert is identifier-idempotent"
        (statement.sql == "INSERT INTO article_outbox "
            <> "(identifier, article_identifier, event_kind, payload, expected_revision) "
            <> "VALUES (?, ?, ?, ?, ?) "
            <> "ON CONFLICT(identifier) DO NOTHING RETURNING identifier")
    check "outbox insert binds event and article data"
        (statement.parameters ==
            [ SQLText "event-1"
            , SQLText (articleIdentifierText (articleIdentifier article))
            , SQLText "ArticleProofreaded"
            , SQLText "{}"
            , SQLNumber 1
            ])

rejectsMalformedRows :: IO ()
rejectsMalformedRows = do
    article <- start
    let identifier = articleIdentifier article
        encoded = articleIdentifierText identifier
    mapM_ (checkRow identifier)
        [ oneRow [SQLText "haskell-syntax", SQLText encoded, SQLText "1"]
        , oneRow [SQLText "haskell-syntax", SQLText encoded, SQLNumber 1.5]
        , oneRow [SQLText "haskell-syntax", SQLText encoded, SQLNumber 9007199254740992]
        , oneRow [SQLText "haskell-syntax", SQLText encoded, SQLNumber 1, SQLNull]
        , SQLResult [] [[], []] 0 2
        ]
  where
    checkRow identifier row = do
        versions <- newVersions
        (execute, _) <- newScript [row]
        result <- findArticleWith execute versions fixtureCodec identifier
        check "malformed article row rejected" (isUnexpected result)

rejectsCodecMismatch :: IO ()
rejectsCodecMismatch = do
    article <- start
    let identifier = articleIdentifier article
        other = "01ARZ3NDEKTSV4RRFFQ69G5FAW"
    versions <- newVersions
    (execute, _) <- newScript
        [oneRow [SQLText "haskell-syntax", SQLText other, SQLNumber 1]]
    mismatched <- findArticleWith execute versions fixtureCodec identifier
    checkError "decoded identifier must match storage key"
        (createUnexpectedError "ArticleStorage"
            "decoded article identifier differs from the stored key") mismatched
    versions2 <- newVersions
    (execute2, _) <- newScript
        [oneRow [SQLText "haskell-syntax", SQLText "not-an-identifier", SQLNumber 1]]
    invalid <- findArticleWith execute2 versions2 fixtureCodec identifier
    check "invalid payload is not trusted" (isUnexpected invalid)

rejectsMissingObservation :: IO ()
rejectsMissingObservation = do
    versions <- newVersions
    (execute, _) <- newScript []
    identifier <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV")
    deleted <- terminateArticleWith execute versions identifier
    check "terminate requires an observed snapshot" $
        case deleted of
            Left (AggregateNotFound _) -> True
            _ -> False

rejectsStaleDelete :: IO ()
rejectsStaleDelete = do
    article <- start
    versions <- newVersions
    let identifier = articleIdentifier article
    (execute, _) <- newScript
        [ oneRow [SQLText "haskell-syntax", SQLText (articleIdentifierText identifier), SQLNumber 1]
        , emptyResult
        ]
    _ <- findArticleWith execute versions fixtureCodec identifier
    deleted <- terminateArticleWith execute versions identifier
    check "stale deletion rejected" (isProcessingTargetChanged deleted)

rejectsUnchangedConflict :: IO ()
rejectsUnchangedConflict = do
    article <- start
    versions <- newVersions
    (execute, _) <- newScript [emptyResult, emptyResult, emptyResult]
    result <- persistArticleWith execute versions fixtureCodec article
    check "unexplained insert conflict is stale" (isProcessingTargetChanged result)

rejectsMalformedOutbox :: IO ()
rejectsMalformedOutbox = do
    article <- start
    let record = OutboxRecord "" (articleIdentifier article) "ArticleProofreaded" "{}" Nothing
    (execute, _) <- newScript []
    result <- appendOutboxWith execute record
    check "empty outbox identifier rejected" (isUnexpected result)
    let valid = record{identifier = "event-2"}
    (execute2, _) <- newScript [oneRow [SQLText "other-event"]]
    malformed <- appendOutboxWith execute2 valid
    check "outbox echo must match identifier" (isUnexpected malformed)

rejectsRevisionOverflow :: IO ()
rejectsRevisionOverflow = do
    article <- start
    versions <- newVersions
    let identifier = articleIdentifier article
        encoded = articleIdentifierText identifier
    (execute, _) <- newScript
        [oneRow [SQLText "haskell-syntax", SQLText encoded, SQLNumber 9007199254740991]]
    _ <- findArticleWith execute versions fixtureCodec identifier
    result <- persistArticleWith execute versions fixtureCodec article
    checkError "revision does not exceed JS safe integer"
        (createOperationNotAllowed "Article" "storage revision limit reached") result

rejectsSchemaFailure :: IO ()
rejectsSchemaFailure = do
    let unavailable _ = throwIO (SQLError "storage unavailable")
    first <- initializeSchemaWith unavailable fixtureCodec
    check "first schema failure is reported" (isServiceUnavailable first)
    count <- newIORef (0 :: Int)
    let second _ = do
            step <- atomicModifyIORef' count $ \value -> (value + 1, value)
            if step == 0 then pure emptyResult else throwIO (SQLError "index unavailable")
    later <- initializeSchemaWith second fixtureCodec
    check "later schema failure is reported" (isServiceUnavailable later)
    check "failed schema creation stops before the index" =<< (== 2) <$> readIORef count
    article <- start
    versions <- newVersions
    lookupFailure <- findArticleWith unavailable versions fixtureCodec (articleIdentifier article)
    check "read SQL failure is reported" (isServiceUnavailable lookupFailure)

rejectsChangedObservation :: IO ()
rejectsChangedObservation = do
    article <- start
    versions <- newVersions
    let identifier = articleIdentifier article
        row revision = oneRow
            [ SQLText "haskell-syntax"
            , SQLText (articleIdentifierText identifier)
            , SQLNumber revision
            ]
    (execute, _) <- newScript [row 1, row 2]
    first <- findArticleWith execute versions fixtureCodec identifier
    check "first observation succeeds" (case first of Right (Just _) -> True; _ -> False)
    second <- findArticleWith execute versions fixtureCodec identifier
    check "changed revision in one transaction is rejected" (isProcessingTargetChanged second)

rejectsEncodingFailure :: IO ()
rejectsEncodingFailure = do
    article <- start
    versions <- newVersions
    let codec = fixtureCodec
            { encodeArticle = \_ -> Left (createServiceUnavailable "Codec" "cannot encode") }
    (execute, statements) <- newScript []
    result <- persistArticleWith execute versions codec article
    check "encoding failure is returned" (isServiceUnavailable result)
    check "failed encoding does not write" . null =<< statements

rejectsMalformedConflictLookups :: IO ()
rejectsMalformedConflictLookups = do
    article <- start
    versions <- newVersions
    (execute, _) <- newScript [emptyResult, oneRow [SQLText "bad-version"]]
    conflict <- persistArticleWith execute versions fixtureCodec article
    check "invalid revision lookup is rejected" (isUnexpected conflict)
    versions2 <- newVersions
    (execute2, _) <- newScript [emptyResult, emptyResult, oneRow [SQLNumber 1]]
    owner <- persistArticleWith execute2 versions2 fixtureCodec article
    checkError "invalid slug owner row is rejected"
        (createUnexpectedError "ArticleStorage"
            "slug lookup returned an invalid row shape") owner
    versions3 <- newVersions
    (execute3, _) <- newScript [emptyResult, SQLResult [] [[], []] 0 2]
    revisionShape <- persistArticleWith execute3 versions3 fixtureCodec article
    checkError "multiple revision lookup rows are rejected"
        (createUnexpectedError "ArticleStorage"
            "revision lookup returned an invalid row shape") revisionShape

persistsSluglessDraft :: IO ()
persistsSluglessDraft = do
    identifier <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV")
    content <- right $ newDraftContent (const (Right Set.empty))
        (DraftInput "Title only" "" Nothing [])
    article <- Unvalidated <$> right (newUnvalidatedDraft identifier timestamp content)
    versions <- newVersions
    (execute, statements) <- newScript [oneRow [SQLNumber 1]]
    result <- persistArticleWith execute versions fixtureCodec article
    check "draft without slug can be inserted" (result == Right ())
    issued <- statements
    statement <- firstStatement issued
    check "missing slug is stored as NULL" (SQLNull `elem` statement.parameters)

rejectsOutboxSQLFailure :: IO ()
rejectsOutboxSQLFailure = do
    article <- start
    let record = OutboxRecord "event-1" (articleIdentifier article)
            "ExcerptGenerationRequested" "{}" Nothing
        unavailable _ = throwIO (SQLError "disk unavailable")
    result <- appendOutboxWith unavailable record
    check "outbox SQL failure is returned" (isServiceUnavailable result)

rejectsSQLFailures :: IO ()
rejectsSQLFailures = do
    article <- start
    let identifier = articleIdentifier article
        unavailable = createServiceUnavailable
            "ArticleStorage" (Text.pack (show (SQLError "storage unavailable")))
    (schemaSQL, schemaStatements) <- newFailingScript 3 (replicate 2 emptyResult)
    schema <- initializeSchemaWith schemaSQL fixtureCodec
    checkError "index SQL failure is returned" unavailable schema
    check "schema stops at failed index" . (== 2) . length =<< schemaStatements
    versions <- newVersions
    (readSQL, _) <- newFailingScript 1 []
    found <- findArticleWith readSQL versions fixtureCodec identifier
    checkError "find SQL failure is returned" unavailable found
    beforeInsert <- readIORef versions
    check "failed find does not record an observation"
        (persistenceMode identifier beforeInsert == Right Insert)
    (insertSQL, _) <- newFailingScript 1 []
    inserted <- persistArticleWith insertSQL versions fixtureCodec article
    checkError "insert SQL failure is returned" unavailable inserted
    afterInsert <- readIORef versions
    check "failed insert retains insert mode"
        (persistenceMode identifier afterInsert == Right Insert)
    updateVersions <- newVersions
    (updateSQL, _) <- newFailingScript 2 [storedRow article 1]
    _ <- findArticleWith updateSQL updateVersions fixtureCodec identifier
    updated <- persistArticleWith updateSQL updateVersions fixtureCodec article
    checkError "update SQL failure is returned" unavailable updated
    afterUpdate <- readIORef updateVersions
    check "failed update keeps the observed revision"
        (persistenceMode identifier afterUpdate == Right (Update initialVersion))
    deleteVersions <- newVersions
    (deleteSQL, _) <- newFailingScript 2 [storedRow article 1]
    _ <- findArticleWith deleteSQL deleteVersions fixtureCodec identifier
    deleted <- terminateArticleWith deleteSQL deleteVersions identifier
    checkError "delete SQL failure is returned" unavailable deleted
    afterDelete <- readIORef deleteVersions
    check "failed delete keeps the aggregate live"
        (persistenceMode identifier afterDelete == Right (Update initialVersion))
    (revisionSQL, _) <- newFailingScript 2 [emptyResult]
    revision <- persistArticleWith revisionSQL versions fixtureCodec article
    checkError "revision lookup SQL failure is returned" unavailable revision
    (slugSQL, _) <- newFailingScript 3 [emptyResult, emptyResult]
    slug <- persistArticleWith slugSQL versions fixtureCodec article
    checkError "slug lookup SQL failure is returned" unavailable slug

rejectsMalformedWriteReturns :: IO ()
rejectsMalformedWriteReturns = do
    article <- start
    let identifier = articleIdentifier article
        malformed = createUnexpectedError "ArticleStorage" "write returned an unexpected row"
        badReturns =
            [ oneRow [SQLNull]
            , oneRow [SQLNumber 1, SQLNumber 1]
            , SQLResult [] [[SQLNumber 1], [SQLNumber 1]] 0 2
            ]
    mapM_ (\row -> do
        versions <- newVersions
        (execute, _) <- newScript [row]
        result <- persistArticleWith execute versions fixtureCodec article
        checkError "malformed insert echo is rejected" malformed result
        context <- readIORef versions
        check "malformed insert does not advance revision"
            (persistenceMode identifier context == Right Insert)) badReturns
    versions <- newVersions
    (updateSQL, _) <- newScript [storedRow article 1, oneRow [SQLNumber 3]]
    _ <- findArticleWith updateSQL versions fixtureCodec identifier
    updated <- persistArticleWith updateSQL versions fixtureCodec article
    checkError "wrong update revision is rejected" malformed updated
    context <- readIORef versions
    check "malformed update does not advance revision"
        (persistenceMode identifier context == Right (Update initialVersion))
    deleteVersions <- newVersions
    (deleteSQL, _) <- newScript
        [storedRow article 1, oneRow [SQLText "wrong-identifier"]]
    _ <- findArticleWith deleteSQL deleteVersions fixtureCodec identifier
    deleted <- terminateArticleWith deleteSQL deleteVersions identifier
    checkError "wrong delete identifier is rejected" malformed deleted
    deleteContext <- readIORef deleteVersions
    check "malformed delete does not terminate the aggregate"
        (persistenceMode identifier deleteContext == Right (Update initialVersion))
    let record = OutboxRecord "event-1" identifier "ArticleProofreaded" "{}" Nothing
    (outboxSQL, _) <- newScript [SQLResult [] [[SQLText "event-1"], [SQLText "event-1"]] 0 2]
    appended <- appendOutboxWith outboxSQL record
    checkError "multiple outbox echoes are rejected" malformed appended

diagnosesUpdateConflicts :: IO ()
diagnosesUpdateConflicts = do
    article <- start
    let identifier = articleIdentifier article
        owner = articleIdentifierText identifier
        stale = createProcessingTargetChanged "Article" "the observed article changed"
        occupied = createOperationNotAllowed "Slug" "slug already belongs to another article"
        cases =
            [ ("revision disappeared", [emptyResult], stale, 3)
            , ("same revision and owner", [oneRow [SQLNumber 1], oneRow [SQLText owner]], stale, 4)
            , ("same revision and another owner", [oneRow [SQLNumber 1], oneRow [SQLText "other"]], occupied, 4)
            ]
    mapM_ (\(name, lookups, expected, statementCount) -> do
        versions <- newVersions
        (execute, statements) <- newScript
            ([storedRow article 1, emptyResult] <> lookups)
        _ <- findArticleWith execute versions fixtureCodec identifier
        result <- persistArticleWith execute versions fixtureCodec article
        checkError name expected result
        issued <- statements
        check (name <> " uses the expected lookups") (length issued == statementCount)
        let revisionLookup = issued !! 2
        check (name <> " looks up the current revision by identifier")
            (revisionLookup.sql ==
                "SELECT revision FROM article_aggregates WHERE identifier = ? LIMIT 2"
                && revisionLookup.parameters == [SQLText owner])
        context <- readIORef versions
        check (name <> " leaves the observed revision intact")
            (persistenceMode identifier context == Right (Update initialVersion))) cases

diagnosesSlugConflicts :: IO ()
diagnosesSlugConflicts = do
    article <- start
    let identifier = articleIdentifier article
        stale = createProcessingTargetChanged "Article" "the observed article changed"
        existing = createOperationNotAllowed "Article" "identifier already exists"
    (sameOwnerSQL, sameOwnerStatements) <- newScript
        [emptyResult, emptyResult, oneRow [SQLText (articleIdentifierText identifier)]]
    versions <- newVersions
    sameOwner <- persistArticleWith sameOwnerSQL versions fixtureCodec article
    checkError "insert conflict with same slug owner remains stale" stale sameOwner
    issued <- sameOwnerStatements
    ownerLookup <- lastStatement issued
    check "slug owner lookup uses the attempted slug"
        (ownerLookup.sql ==
            "SELECT identifier FROM article_aggregates WHERE slug = ? LIMIT 2"
            && ownerLookup.parameters == [SQLText "haskell-syntax"])
    (existingSQL, _) <- newScript [emptyResult, oneRow [SQLNumber 1]]
    alreadyExists <- persistArticleWith existingSQL versions fixtureCodec article
    checkError "identifier conflict is diagnosed before slug lookup" existing alreadyExists
    sluglessIdentifier <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV")
    sluglessContent <- right $ newDraftContent (const (Right Set.empty))
        (DraftInput "No slug" "" Nothing [])
    slugless <- Unvalidated <$> right
        (newUnvalidatedDraft sluglessIdentifier timestamp sluglessContent)
    (sluglessSQL, sluglessStatements) <- newScript [emptyResult, emptyResult]
    sluglessVersions <- newVersions
    noSlug <- persistArticleWith sluglessSQL sluglessVersions fixtureCodec slugless
    checkError "slugless insert conflict is stale" stale noSlug
    check "slugless conflict skips owner lookup"
        . (== 2) . length =<< sluglessStatements

observesRepeatedReads :: IO ()
observesRepeatedReads = do
    article <- start
    let identifier = articleIdentifier article
        stale = createProcessingTargetChanged "Article" "the observed aggregate changed"
    versions <- newVersions
    (execute, _) <- newScript [storedRow article 1, storedRow article 1]
    first <- findArticleWith execute versions fixtureCodec identifier
    second <- findArticleWith execute versions fixtureCodec identifier
    check "same revision can be read twice"
        (fmap (fmap articleIdentifier) first == Right (Just identifier)
            && fmap (fmap articleIdentifier) second == Right (Just identifier))
    missingVersions <- newVersions
    (missingSQL, _) <- newScript [emptyResult, storedRow article 1]
    _ <- findArticleWith missingSQL missingVersions fixtureCodec identifier
    appeared <- findArticleWith missingSQL missingVersions fixtureCodec identifier
    checkError "article appearing after missing observation is stale" stale appeared
    (vanishingSQL, _) <- newScript [storedRow article 1, emptyResult]
    vanishingVersions <- newVersions
    _ <- findArticleWith vanishingSQL vanishingVersions fixtureCodec identifier
    vanished <- findArticleWith vanishingSQL vanishingVersions fixtureCodec identifier
    checkError "article disappearing after observation is stale" stale vanished

rejectsAdditionalMalformedRows :: IO ()
rejectsAdditionalMalformedRows = do
    article <- start
    let identifier = articleIdentifier article
        encoded = articleIdentifierText identifier
        invalidShape = createUnexpectedError
            "ArticleStorage" "article query returned an invalid row shape"
        invalidRevision = createUnexpectedError
            "ArticleStorage" "revision is not a safe positive integer"
        invalidSlug = createUnexpectedError
            "ArticleStorage" "decoded article slug differs from the stored index"
        cases =
            [ ("null payload", oneRow [SQLText "haskell-syntax", SQLNull, SQLNumber 1], invalidShape)
            , ("missing column", oneRow [SQLText "haskell-syntax", SQLText encoded], invalidShape)
            , ("two rows", SQLResult []
                [[SQLText "haskell-syntax", SQLText encoded, SQLNumber 1],
                 [SQLText "haskell-syntax", SQLText encoded, SQLNumber 1]] 0 2, invalidShape)
            , ("negative revision", storedRow article (-1), invalidRevision)
            , ("not-a-number revision", storedRow article (0 / 0), invalidRevision)
            , ("infinite revision", storedRow article (1 / 0), invalidRevision)
            , ("null slug", oneRow [SQLNull, SQLText encoded, SQLNumber 1], invalidSlug)
            ]
    mapM_ (\(name, row, expected) -> do
        versions <- newVersions
        (execute, _) <- newScript [row]
        result <- findArticleWith execute versions fixtureCodec identifier
        checkError name expected result
        context <- readIORef versions
        check (name <> " is not observed")
            (persistenceMode identifier context == Right Insert)) cases
    let badCodec = fixtureCodec
            { decodeArticle = \_ -> Left
                (createServiceUnavailable "Codec" "cannot decode") }
    versions <- newVersions
    (execute, _) <- newScript [storedRow article 1]
    decoded <- findArticleWith execute versions badCodec identifier
    checkError "codec failure becomes a corrupt payload error"
        (createUnexpectedError "ArticleStorage" "article payload fails domain validation") decoded
    sluglessContent <- right $ newDraftContent (const (Right Set.empty))
        (DraftInput "No slug" "" Nothing [])
    sluglessDraft <- right (newUnvalidatedDraft identifier timestamp sluglessContent)
    let slugless = Unvalidated sluglessDraft
        sluglessCodec = fixtureCodec{decodeArticle = \_ -> Right slugless}
    sluglessVersions <- newVersions
    (sluglessSQL, _) <- newScript
        [oneRow [SQLNull, SQLText encoded, SQLNumber 1]]
    restored <- findArticleWith sluglessSQL sluglessVersions sluglessCodec identifier
    check "null stored slug matches a slugless draft"
        (fmap (fmap articleIdentifier) restored == Right (Just identifier))

validatesOutboxInputs :: IO ()
validatesOutboxInputs = do
    article <- start
    let record = OutboxRecord "event-1" (articleIdentifier article)
            "ArticleProofreaded" "{}" Nothing
        required = createUnexpectedError "Outbox" "identifier and event kind are required"
        duplicate = createOperationNotAllowed "Outbox" "event identifier already exists"
    (invalidSQL, invalidStatements) <- newScript []
    invalid <- appendOutboxWith invalidSQL record{eventKind = ""}
    checkError "empty event kind is rejected" required invalid
    check "invalid outbox input never writes" . null =<< invalidStatements
    (validSQL, validStatements) <- newScript [oneRow [SQLText "event-1"], emptyResult]
    appended <- appendOutboxWith validSQL record
    check "outbox without expected revision succeeds" (appended == Right ())
    duplicateResult <- appendOutboxWith validSQL record
    checkError "duplicate outbox id reports its cause" duplicate duplicateResult
    issued <- validStatements
    statement <- firstStatement issued
    check "outbox stores absent expected revision as NULL"
        (last statement.parameters == SQLNull)

persistsLifecycleSlugs :: IO ()
persistsLifecycleSlugs = do
    identifier <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV")
    content <- right $ newDraftContent (const (Right Set.empty))
        (DraftInput "Lifecycle" "Body" (Just "haskell-syntax") [])
    draft <- right (newUnvalidatedDraft identifier timestamp content)
    available <- right (confirmAvailableImageReferences Set.empty Set.empty)
    proofreaded <- right (proofread timestamp available draft)
    excerpt <- right (newExcerpt "Summary")
    ready <- right (prepareToPublish timestamp excerpt proofreaded)
    published <- right (publish timestamp ready)
    private <- right (takeDown timestamp published)
    mapM_ (\(name, article, hasPublicationTime) -> do
        versions <- newVersions
        (execute, statements) <- newScript [oneRow [SQLNumber 1]]
        result <- persistArticleWith execute versions fixtureCodec article
        check (name <> " can be persisted") (result == Right ())
        statement <- firstStatement =<< statements
        case statement.parameters of
            [_identifier, storedSlug, phase, updated, publication, _payload] -> do
                check (name <> " retains its slug")
                    (storedSlug == SQLText "haskell-syntax")
                check (name <> " indexes its lifecycle phase")
                    (phase == SQLText (Text.pack name))
                check (name <> " indexes its update time")
                    (case updated of
                        SQLText value -> "20260101" `Text.isPrefixOf` value
                        _ -> False)
                check (name <> " indexes its publication time")
                    (case (hasPublicationTime, publication) of
                        (False, SQLNull) -> True
                        (True, SQLText value) -> "20260101" `Text.isPrefixOf` value
                        _ -> False)
            _ -> fail "expected lifecycle insert parameters")
        [ ("proofreaded", Proofreaded proofreaded, False)
        , ("ready", Ready ready, False)
        , ("published", Published published, True)
        , ("private", Private private, True)
        ]

isServiceUnavailable :: Either DomainError a -> Bool
isServiceUnavailable (Left (ServiceUnavailable _)) = True
isServiceUnavailable _ = False

isOperationNotAllowed :: Either DomainError a -> Bool
isOperationNotAllowed (Left (OperationNotAllowed _)) = True
isOperationNotAllowed _ = False

isProcessingTargetChanged :: Either DomainError a -> Bool
isProcessingTargetChanged (Left (ProcessingTargetChanged _)) = True
isProcessingTargetChanged _ = False

isUnexpected :: Either DomainError a -> Bool
isUnexpected (Left (UnexpectedError _)) = True
isUnexpected _ = False
