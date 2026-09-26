{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.DurableObject.GenerationJob (
    GenerationRequestDecision (..),
    GenerationClaim (..),
    GenerationFinalization (..),
    initializeGenerationJobSchemaWith,
    requestGenerationWith,
    claimGenerationWith,
    completeGenerationWith,
    abandonGenerationWith,
) where

import Cloudflare.Workers.Binding.DurableObject.SQL (
    SQLError,
    SQLResult (..),
    SQLStatement (..),
    SQLValue (..),
 )
import Control.Exception (try)
import Data.Text (Text)
import Data.Text qualified as Text
import "article" Domain.Article.Common (ArticleIdentifier, articleIdentifierText)
import Infrastructure.Article.DurableObject.Repository (ExecuteSQL)
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerated (..),
    ExcerptGenerationRequested (..),
    GenerationRequestIdentifier,
    generationRequestIdentifierText,
    newGenerationRequestIdentifier,
 )
import "shared" Shared.Domain.Common.Primitive (newPositiveInteger)
import "shared" Shared.Domain.Error (
    DomainError,
    createProcessingTargetChanged,
    createServiceUnavailable,
    createUnexpectedError,
 )
import "shared" Shared.Infrastructure.Versioning (Version, newVersion, versionInteger)

data GenerationRequestDecision
    = GenerationCreated ExcerptGenerationRequested
    | GenerationReused ExcerptGenerationRequested
    deriving stock (Show, Eq)

data GenerationClaim = GenerationClaimed | GenerationTerminalAck
    deriving stock (Show, Eq)

data GenerationFinalization = GenerationFinalized | GenerationFinalizationTerminalAck
    deriving stock (Show, Eq)

-- Pass the transaction-local ExecuteSQL supplied by the caller. A Left from
-- any operation must cause that caller's DO transaction to roll back.
initializeGenerationJobSchemaWith :: ExecuteSQL -> IO (Either DomainError ())
initializeGenerationJobSchemaWith execute = do
    created <- executeSQL execute tableSchema []
    case created of
        Left err -> pure (Left err)
        Right _ -> do
            indexed <- executeSQL execute activeIndex []
            pure (() <$ indexed)
  where
    tableSchema =
        "CREATE TABLE IF NOT EXISTS article_generation_jobs ("
            <> "request_identifier TEXT PRIMARY KEY, "
            <> "article_identifier TEXT NOT NULL, "
            <> "revision INTEGER NOT NULL CHECK (revision > 0 AND revision <= 9007199254740991), "
            <> "status TEXT NOT NULL CHECK (status IN ('active', 'completed', 'superseded', 'failed')))"
    activeIndex =
        "CREATE UNIQUE INDEX IF NOT EXISTS article_generation_jobs_active "
            <> "ON article_generation_jobs (article_identifier) WHERE status = 'active'"

-- The offered identifier is generated outside the infrastructure layer. It is
-- unused when an active request for the same article/revision already exists.
requestGenerationWith ::
    ExecuteSQL ->
    ArticleIdentifier ->
    Version ->
    GenerationRequestIdentifier ->
    IO (Either DomainError GenerationRequestDecision)
requestGenerationWith execute article expectedRevision offered = do
    current <- currentArticleRevision execute article
    case current of
        Left err -> pure (Left err)
        Right revision | revision /= Just expectedRevision -> pure (Left changed)
        Right _ -> do
            active <- activeRequest execute article
            case active of
                Left err -> pure (Left err)
                Right (Just (identifier, revision))
                    | revision == expectedRevision ->
                        pure (Right (GenerationReused (request identifier)))
                    | versionInteger revision > versionInteger expectedRevision ->
                        pure (Left changed)
                    | otherwise -> do
                        superseded <-
                            executeSQL
                                execute
                                ( "UPDATE article_generation_jobs SET status = 'superseded' "
                                    <> "WHERE request_identifier = ? AND article_identifier = ? "
                                    <> "AND revision = ? AND status = 'active' RETURNING request_identifier"
                                )
                                [ requestValue identifier
                                , articleValue article
                                , revisionValue revision
                                ]
                        case superseded >>= returnedIdentifier identifier of
                            Left err -> pure (Left err)
                            Right False -> pure (Left changed)
                            Right True -> insertNew
                Right Nothing -> insertNew
  where
    request identifier = ExcerptGenerationRequested identifier article expectedRevision
    insertNew = do
        inserted <-
            executeSQL
                execute
                ( "INSERT INTO article_generation_jobs "
                    <> "(request_identifier, article_identifier, revision, status) "
                    <> "SELECT ?, ?, ?, 'active' WHERE EXISTS "
                    <> "(SELECT 1 FROM article_aggregates WHERE identifier = ? AND revision = ?) "
                    <> "ON CONFLICT DO NOTHING RETURNING request_identifier"
                )
                [ requestValue offered
                , articleValue article
                , revisionValue expectedRevision
                , articleValue article
                , revisionValue expectedRevision
                ]
        pure $ case inserted >>= returnedIdentifier offered of
            Left err -> Left err
            Right False -> Left changed
            Right True -> Right (GenerationCreated (request offered))

-- Terminal, missing, or obsolete requests all acknowledge without AI work.
-- The aggregate's current storage revision must still match the request.
claimGenerationWith ::
    ExecuteSQL -> ExcerptGenerationRequested -> IO (Either DomainError GenerationClaim)
claimGenerationWith execute request = do
    selected <-
        executeSQL
            execute
            ( "SELECT job.request_identifier FROM article_generation_jobs AS job "
                <> "JOIN article_aggregates AS article "
                <> "ON article.identifier = job.article_identifier "
                <> "WHERE job.request_identifier = ? AND job.article_identifier = ? "
                <> "AND job.revision = ? AND job.status = 'active' "
                <> "AND article.revision = ? LIMIT 2"
            )
            [ requestValue request.identifier
            , articleValue request.article
            , revisionValue request.expectedRevision
            , revisionValue request.expectedRevision
            ]
    pure $ case selected >>= returnedIdentifier request.identifier of
        Left err -> Left err
        Right False -> Right GenerationTerminalAck
        Right True -> Right GenerationClaimed

-- The callback must persist the completed article in the same DO transaction.
-- A failed callback or missing revision increment cannot become a successful
-- terminal result; its Left must roll the entire transaction back.
completeGenerationWith ::
    ExecuteSQL ->
    ExcerptGenerated ->
    IO (Either DomainError ()) ->
    IO (Either DomainError GenerationFinalization)
completeGenerationWith execute generated persistCompleted = do
    claim <- claimGenerationWith execute request
    case claim of
        Left err -> pure (Left err)
        Right GenerationTerminalAck -> pure (Right GenerationFinalizationTerminalAck)
        Right GenerationClaimed ->
            case followingRevision generated.expectedRevision of
                Left err -> pure (Left err)
                Right revision -> do
                    persisted <- persistCompleted
                    case persisted of
                        Left err -> pure (Left err)
                        Right () -> do
                            marked <-
                                executeSQL
                                    execute
                                    ( "UPDATE article_generation_jobs SET status = 'completed' "
                                        <> "WHERE request_identifier = ? AND article_identifier = ? "
                                        <> "AND revision = ? AND status = 'active' AND EXISTS "
                                        <> "(SELECT 1 FROM article_aggregates WHERE identifier = ? "
                                        <> "AND revision = ?) RETURNING request_identifier"
                                    )
                                    [ requestValue generated.request
                                    , articleValue generated.article
                                    , revisionValue generated.expectedRevision
                                    , articleValue generated.article
                                    , revisionValue revision
                                    ]
                            pure $ case marked >>= returnedIdentifier generated.request of
                                Left err -> Left err
                                Right False -> Left incompleteCompletion
                                Right True -> Right GenerationFinalized
  where
    request =
        ExcerptGenerationRequested
            generated.request
            generated.article
            generated.expectedRevision

-- A DLQ decision may release an active request for a later explicit retry.
-- It never marks an article as having a generated excerpt.
abandonGenerationWith ::
    ExecuteSQL -> ExcerptGenerationRequested -> IO (Either DomainError GenerationFinalization)
abandonGenerationWith execute request = do
    claim <- claimGenerationWith execute request
    case claim of
        Left err -> pure (Left err)
        Right GenerationTerminalAck -> pure (Right GenerationFinalizationTerminalAck)
        Right GenerationClaimed -> do
            marked <-
                executeSQL
                    execute
                    ( "UPDATE article_generation_jobs SET status = 'failed' "
                        <> "WHERE request_identifier = ? AND article_identifier = ? "
                        <> "AND revision = ? AND status = 'active' AND EXISTS "
                        <> "(SELECT 1 FROM article_aggregates WHERE identifier = ? "
                        <> "AND revision = ?) RETURNING request_identifier"
                    )
                    [ requestValue request.identifier
                    , articleValue request.article
                    , revisionValue request.expectedRevision
                    , articleValue request.article
                    , revisionValue request.expectedRevision
                    ]
            pure $ case marked >>= returnedIdentifier request.identifier of
                Left err -> Left err
                Right False -> Left changed
                Right True -> Right GenerationFinalized

currentArticleRevision :: ExecuteSQL -> ArticleIdentifier -> IO (Either DomainError (Maybe Version))
currentArticleRevision execute article = do
    found <-
        executeSQL
            execute
            "SELECT revision FROM article_aggregates WHERE identifier = ? LIMIT 2"
            [articleValue article]
    pure $ case found of
        Left err -> Left err
        Right result -> case result.rows of
            [] -> Right Nothing
            [[value]] -> Just <$> decodeRevision value
            _ -> Left corruptRow

activeRequest ::
    ExecuteSQL -> ArticleIdentifier -> IO (Either DomainError (Maybe (GenerationRequestIdentifier, Version)))
activeRequest execute article = do
    found <-
        executeSQL
            execute
            ( "SELECT request_identifier, revision FROM article_generation_jobs "
                <> "WHERE article_identifier = ? AND status = 'active' LIMIT 2"
            )
            [articleValue article]
    pure $ case found of
        Left err -> Left err
        Right result -> case result.rows of
            [] -> Right Nothing
            [[SQLText rawIdentifier, rawRevision]] -> do
                identifier <-
                    either
                        (const (Left corruptRow))
                        Right
                        (newGenerationRequestIdentifier rawIdentifier)
                revision <- decodeRevision rawRevision
                Right (Just (identifier, revision))
            _ -> Left corruptRow

executeSQL :: ExecuteSQL -> Text -> [SQLValue] -> IO (Either DomainError SQLResult)
executeSQL execute statement parameters = do
    result <- try (execute SQLStatement{sql = statement, parameters})
    pure $ case result of
        Left (err :: SQLError) ->
            Left (createServiceUnavailable "ArticleStorage" (Text.pack (show err)))
        Right value -> Right value

returnedIdentifier :: GenerationRequestIdentifier -> SQLResult -> Either DomainError Bool
returnedIdentifier expected result = case result.rows of
    [] -> Right False
    [[SQLText actual]] | actual == generationRequestIdentifierText expected -> Right True
    _ -> Left corruptRow

decodeRevision :: SQLValue -> Either DomainError Version
decodeRevision (SQLNumber number)
    | not (isNaN number || isInfinite number)
        && number >= 1
        && number <= fromInteger maxSafeInteger
        && fromInteger (round number) == number =
        newVersion <$> newPositiveInteger (round number)
decodeRevision _ = Left corruptRow

followingRevision :: Version -> Either DomainError Version
followingRevision version =
    let following = versionInteger version + 1
     in if following <= maxSafeInteger
            then newVersion <$> newPositiveInteger following
            else Left changed

requestValue :: GenerationRequestIdentifier -> SQLValue
requestValue = SQLText . generationRequestIdentifierText

articleValue :: ArticleIdentifier -> SQLValue
articleValue = SQLText . articleIdentifierText

revisionValue :: Version -> SQLValue
revisionValue = SQLNumber . fromInteger . versionInteger

maxSafeInteger :: Integer
maxSafeInteger = 9007199254740991

changed :: DomainError
changed = createProcessingTargetChanged "ArticleGeneration" "the processing target changed"

corruptRow :: DomainError
corruptRow = createUnexpectedError "ArticleStorage" "generation job query returned an invalid row"

incompleteCompletion :: DomainError
incompleteCompletion =
    createUnexpectedError "ArticleStorage" "generation completion did not persist the next revision"
