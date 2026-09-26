{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.DurableObject.GenerationJobSpec (run) where

import Cloudflare.Workers.Binding.DurableObject.SQL (
    SQLError (..),
    SQLResult (..),
    SQLStatement (..),
    SQLValue (..),
 )
import Control.Exception (evaluate, throwIO)
import Control.Monad (unless)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import "article" Domain.Article.Common (ArticleIdentifier, newArticleIdentifier)
import Infrastructure.Article.DurableObject.GenerationJob
import Infrastructure.Article.DurableObject.Repository (ExecuteSQL)
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerated (..),
    ExcerptGenerationRequested (..),
    GenerationRequestIdentifier,
    newGenerationRequestIdentifier,
 )
import "shared" Shared.Domain.Common.Primitive (newPositiveInteger)
import "shared" Shared.Domain.Error (DomainError (..), createOperationNotAllowed)
import "shared" Shared.Domain.Excerpt (newExcerpt)
import "shared" Shared.Infrastructure.Versioning (Version, newVersion)

check :: String -> Bool -> IO ()
check name condition = unless condition (fail name)

right :: (Show errorValue) => Either errorValue value -> IO value
right = either (fail . show) pure

article :: IO ArticleIdentifier
article = right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV")

identifier :: IO GenerationRequestIdentifier
identifier = right (newGenerationRequestIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")

otherIdentifier :: IO GenerationRequestIdentifier
otherIdentifier = right (newGenerationRequestIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAX")

revision :: Integer -> IO Version
revision value = newVersion <$> right (newPositiveInteger value)

request :: IO ExcerptGenerationRequested
request = ExcerptGenerationRequested <$> identifier <*> article <*> revision 3

generated :: IO ExcerptGenerated
generated = ExcerptGenerated <$> identifier <*> article <*> revision 3 <*> right (newExcerpt "Summary")

emptyResult :: SQLResult
emptyResult = SQLResult [] [] 0 0

oneRow :: [SQLValue] -> SQLResult
oneRow row = SQLResult [] [row] 0 1

newScript :: [SQLResult] -> IO (ExecuteSQL, IO [SQLStatement])
newScript = newOutcomeScript . fmap Right

newOutcomeScript :: [Either SQLError SQLResult] -> IO (ExecuteSQL, IO [SQLStatement])
newOutcomeScript responses = do
    pending <- newIORef responses
    issued <- newIORef []
    let execute statement = do
            _ <- evaluate (length (show statement))
            modifyIORef' issued (<> [statement])
            next <- atomicModifyIORef' pending $ \remaining -> case remaining of
                [] -> ([], Nothing)
                value : rest -> (rest, Just value)
            maybe (fail "unexpected SQL statement") (either throwIO pure) next
    pure (execute, readIORef issued)

isChanged :: Either DomainError value -> Bool
isChanged (Left (ProcessingTargetChanged _)) = True
isChanged _ = False

isUnexpected :: Either DomainError value -> Bool
isUnexpected (Left (UnexpectedError _)) = True
isUnexpected _ = False

isUnavailable :: Either DomainError value -> Bool
isUnavailable (Left (ServiceUnavailable _)) = True
isUnavailable _ = False

sqlFailure :: SQLError
sqlFailure = SQLError "SQL unavailable"

expectSQL :: Text -> SQLStatement -> IO ()
expectSQL fragment statement =
    check ("SQL must contain " <> Text.unpack fragment) (fragment `Text.isInfixOf` statement.sql)

run :: IO ()
run = do
    initializesSchema
    reportsSchemaFailures
    createsRequest
    reusesActiveRequest
    replacesOlderRevision
    rejectsLostSupersede
    rejectsStaleAndMissingArticles
    rejectsConflictingRequestIdentifier
    claimsOnlyCurrentActiveRequest
    completesAfterArticlePersistence
    acknowledgesObsoleteCompletion
    preservesFailures
    rejectsFalseCompletion
    rejectsRevisionOverflow
    abandonsFailedRequest
    rejectsCorruptRows
    reportsRequestSQLFailures
    rejectsFutureAndMalformedActiveRequests
    rejectsMalformedWriteResults
    reportsClaimAndCompletionFailures
    reportsAbandonFailures

initializesSchema :: IO ()
initializesSchema = do
    (execute, issued) <- newScript [emptyResult, emptyResult]
    result <- initializeGenerationJobSchemaWith execute
    check "schema initialized" (result == Right ())
    statements <- issued
    check "table and unique index created" (length statements == 2)
    case statements of
        [table, index] -> do
            expectSQL "CREATE TABLE IF NOT EXISTS article_generation_jobs" table
            expectSQL "CHECK (revision > 0" table
            expectSQL "CHECK (status IN" table
            expectSQL "CREATE UNIQUE INDEX IF NOT EXISTS" index
            expectSQL "WHERE status = 'active'" index
        _ -> fail "expected two schema statements"

reportsSchemaFailures :: IO ()
reportsSchemaFailures = do
    (tableSQL, tableIssued) <- newOutcomeScript [Left sqlFailure]
    tableResult <- initializeGenerationJobSchemaWith tableSQL
    check "table creation failure is retryable" (isUnavailable tableResult)
    check "index is not attempted after failed table creation" . (== 1) . length =<< tableIssued

    (indexSQL, indexIssued) <- newOutcomeScript [Right emptyResult, Left sqlFailure]
    indexResult <- initializeGenerationJobSchemaWith indexSQL
    check "index creation failure is retryable" (isUnavailable indexResult)
    check "index failure follows successful table creation" . (== 2) . length =<< indexIssued

createsRequest :: IO ()
createsRequest = do
    target <- article
    proposed <- identifier
    version <- revision 3
    (execute, issued) <-
        newScript
            [ oneRow [SQLNumber 3]
            , emptyResult
            , oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]
            ]
    result <- requestGenerationWith execute target version proposed
    check
        "new active request created"
        (result == Right (GenerationCreated (ExcerptGenerationRequested proposed target version)))
    statements <- issued
    check "three SQL operations" (length statements == 3)
    case reverse statements of
        inserted : _ -> do
            expectSQL "INSERT INTO article_generation_jobs" inserted
            expectSQL "WHERE EXISTS" inserted
            expectSQL "ON CONFLICT DO NOTHING RETURNING" inserted
            check
                "insert binds article and revision for both insert and guard"
                ( inserted.parameters
                    == [ SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"
                       , SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAV"
                       , SQLNumber 3
                       , SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAV"
                       , SQLNumber 3
                       ]
                )
        _ -> fail "missing insert"

reusesActiveRequest :: IO ()
reusesActiveRequest = do
    target <- article
    existing <- identifier
    offered <- otherIdentifier
    version <- revision 3
    (execute, issued) <-
        newScript
            [oneRow [SQLNumber 3], oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW", SQLNumber 3]]
    result <- requestGenerationWith execute target version offered
    check
        "same article/revision reuses existing request"
        (result == Right (GenerationReused (ExcerptGenerationRequested existing target version)))
    check "reuse does not insert offered identifier" . (== 2) . length =<< issued

replacesOlderRevision :: IO ()
replacesOlderRevision = do
    target <- article
    offered <- otherIdentifier
    version <- revision 4
    (execute, issued) <-
        newScript
            [ oneRow [SQLNumber 4]
            , oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW", SQLNumber 3]
            , oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]
            , oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAX"]
            ]
    result <- requestGenerationWith execute target version offered
    check
        "new revision replaces older active request"
        (result == Right (GenerationCreated (ExcerptGenerationRequested offered target version)))
    statements <- issued
    check "supersede and insert happen in caller transaction" (length statements == 4)
    case drop 2 statements of
        [supersede, inserted] -> do
            expectSQL "SET status = 'superseded'" supersede
            expectSQL "AND status = 'active' RETURNING" supersede
            expectSQL "INSERT INTO article_generation_jobs" inserted
        _ -> fail "expected supersede followed by insert"

rejectsLostSupersede :: IO ()
rejectsLostSupersede = do
    target <- article
    offered <- otherIdentifier
    version <- revision 4
    (execute, issued) <-
        newScript
            [ oneRow [SQLNumber 4]
            , oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW", SQLNumber 3]
            , emptyResult
            ]
    result <- requestGenerationWith execute target version offered
    check "lost supersede is a transaction failure" (isChanged result)
    check "no replacement inserted after lost supersede" . (== 3) . length =<< issued

rejectsStaleAndMissingArticles :: IO ()
rejectsStaleAndMissingArticles = do
    target <- article
    offered <- identifier
    version <- revision 3
    (staleSQL, staleIssued) <- newScript [oneRow [SQLNumber 4]]
    stale <- requestGenerationWith staleSQL target version offered
    check "stale revision rejected" (isChanged stale)
    check "stale revision does not touch jobs" . (== 1) . length =<< staleIssued
    (missingSQL, _) <- newScript [emptyResult]
    missing <- requestGenerationWith missingSQL target version offered
    check "missing aggregate rejected" (isChanged missing)

rejectsConflictingRequestIdentifier :: IO ()
rejectsConflictingRequestIdentifier = do
    target <- article
    offered <- identifier
    version <- revision 3
    (execute, _) <- newScript [oneRow [SQLNumber 3], emptyResult, emptyResult]
    result <- requestGenerationWith execute target version offered
    check "identifier conflict cannot masquerade as creation" (isChanged result)

claimsOnlyCurrentActiveRequest :: IO ()
claimsOnlyCurrentActiveRequest = do
    value <- request
    (activeSQL, issued) <- newScript [oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]]
    active <- claimGenerationWith activeSQL value
    check "active matching request claimed" (active == Right GenerationClaimed)
    statements <- issued
    case statements of
        [selection] -> do
            expectSQL "job.status = 'active'" selection
            expectSQL "article.revision = ?" selection
            check "claim binds request, article, revision twice" (length selection.parameters == 4)
        _ -> fail "expected claim query"
    (terminalSQL, _) <- newScript [emptyResult]
    terminal <- claimGenerationWith terminalSQL value
    check "completed, obsolete, or missing request is terminal ack" (terminal == Right GenerationTerminalAck)

completesAfterArticlePersistence :: IO ()
completesAfterArticlePersistence = do
    value <- generated
    persisted <- newIORef False
    (execute, issued) <-
        newScript
            [ oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]
            , oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]
            ]
    result <- completeGenerationWith execute value $ do
        modifyIORef' persisted (const True)
        pure (Right ())
    check "article persistence callback invoked" =<< readIORef persisted
    check "completion finalized" (result == Right GenerationFinalized)
    statements <- issued
    case reverse statements of
        marked : _ -> do
            expectSQL "SET status = 'completed'" marked
            expectSQL "AND revision = ?) RETURNING" marked
            check "completion requires next aggregate revision" (last marked.parameters == SQLNumber 4)
        _ -> fail "expected completed update"

acknowledgesObsoleteCompletion :: IO ()
acknowledgesObsoleteCompletion = do
    value <- generated
    persisted <- newIORef False
    (execute, issued) <- newScript [emptyResult]
    result <- completeGenerationWith execute value $ do
        modifyIORef' persisted (const True)
        pure (Right ())
    check "obsolete message terminal ack" (result == Right GenerationFinalizationTerminalAck)
    check "obsolete message never persists article" . not =<< readIORef persisted
    check "obsolete message does not change job" . (== 1) . length =<< issued

preservesFailures :: IO ()
preservesFailures = do
    value <- generated
    let failure = createOperationNotAllowed "Article" "not proofreaded"
    (execute, issued) <- newScript [oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]]
    result <- completeGenerationWith execute value (pure (Left failure))
    check "article persistence failure is propagated" (result == Left failure)
    check "failed persistence never finalizes job" . (== 1) . length =<< issued

rejectsFalseCompletion :: IO ()
rejectsFalseCompletion = do
    value <- generated
    (execute, _) <-
        newScript [oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"], emptyResult]
    result <- completeGenerationWith execute value (pure (Right ()))
    check "missing revision advance is retryable, not a terminal ack" (isUnexpected result)

rejectsRevisionOverflow :: IO ()
rejectsRevisionOverflow = do
    value <- generated
    maximumRevision <- revision 9007199254740991
    called <- newIORef False
    let exhausted = ExcerptGenerated value.request value.article maximumRevision value.excerpt
    (execute, issued) <- newScript [oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]]
    result <- completeGenerationWith execute exhausted $ do
        modifyIORef' called (const True)
        pure (Right ())
    check "revision overflow is a transaction failure" (isChanged result)
    check "overflow skips article persistence" . not =<< readIORef called
    check "overflow does not complete job" . (== 1) . length =<< issued

abandonsFailedRequest :: IO ()
abandonsFailedRequest = do
    value <- request
    (execute, issued) <-
        newScript
            [ oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]
            , oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]
            ]
    result <- abandonGenerationWith execute value
    check "failed request terminalized" (result == Right GenerationFinalized)
    statements <- issued
    case reverse statements of
        marked : _ -> expectSQL "SET status = 'failed'" marked
        _ -> fail "expected failed update"
    (terminalSQL, _) <- newScript [emptyResult]
    terminal <- abandonGenerationWith terminalSQL value
    check
        "duplicate failure notification is terminal ack"
        (terminal == Right GenerationFinalizationTerminalAck)

rejectsCorruptRows :: IO ()
rejectsCorruptRows = do
    target <- article
    offered <- identifier
    version <- revision 3
    (badVersionSQL, _) <- newScript [oneRow [SQLNumber 1.5]]
    badVersion <- requestGenerationWith badVersionSQL target version offered
    check "fractional article revision rejected" (isUnexpected badVersion)
    (duplicateSQL, _) <-
        newScript
            [ oneRow [SQLNumber 3]
            , SQLResult []
                [ [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW", SQLNumber 3]
                , [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAX", SQLNumber 3]
                ]
                0
                2
            ]
    duplicate <- requestGenerationWith duplicateSQL target version offered
    check "more than one active row is storage corruption" (isUnexpected duplicate)

reportsRequestSQLFailures :: IO ()
reportsRequestSQLFailures = do
    target <- article
    offered <- identifier
    version <- revision 3
    let activeOld = oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW", SQLNumber 2]
        cases =
            [ ("article lookup", [Left sqlFailure])
            , ("active lookup", [Right (oneRow [SQLNumber 3]), Left sqlFailure])
            , ( "supersede"
              , [Right (oneRow [SQLNumber 3]), Right activeOld, Left sqlFailure]
              )
            , ( "new request insert"
              , [Right (oneRow [SQLNumber 3]), Right emptyResult, Left sqlFailure]
              )
            ]
    mapM_
        (\(name, outcomes) -> do
            (execute, _) <- newOutcomeScript outcomes
            result <- requestGenerationWith execute target version offered
            check (name <> " SQL failure is retryable") (isUnavailable result)
        )
        cases

rejectsFutureAndMalformedActiveRequests :: IO ()
rejectsFutureAndMalformedActiveRequests = do
    target <- article
    offered <- identifier
    version <- revision 3
    (futureSQL, futureIssued) <-
        newScript
            [ oneRow [SQLNumber 3]
            , oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW", SQLNumber 4]
            ]
    future <- requestGenerationWith futureSQL target version offered
    check "future active revision is not replaced by stale request" (isChanged future)
    check "future active request remains untouched" . (== 2) . length =<< futureIssued

    let malformed =
            [ ("invalid request identifier", oneRow [SQLText "invalid", SQLNumber 3])
            , ("zero active revision", oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW", SQLNumber 0])
            , ("missing active revision", oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"])
            , ("duplicate active rows", SQLResult []
                [ [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW", SQLNumber 3]
                , [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAX", SQLNumber 3]
                ] 0 2)
            ]
    mapM_
        (\(name, response) -> do
            (execute, _) <- newScript [oneRow [SQLNumber 3], response]
            result <- requestGenerationWith execute target version offered
            check (name <> " is rejected") (isUnexpected result)
        )
        malformed

rejectsMalformedWriteResults :: IO ()
rejectsMalformedWriteResults = do
    target <- article
    offered <- identifier
    version <- revision 3
    let malformedInsert =
            [ ("different returned identifier", oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAX"])
            , ("non-text returned identifier", oneRow [SQLNumber 3])
            , ("two returned identifiers", SQLResult []
                [ [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]
                , [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAX"]
                ] 0 2)
            ]
    mapM_
        (\(name, response) -> do
            (execute, _) <- newScript [oneRow [SQLNumber 3], emptyResult, response]
            result <- requestGenerationWith execute target version offered
            check (name <> " cannot report a created request") (isUnexpected result)
        )
        malformedInsert

    (supersedeSQL, supersedeIssued) <-
        newScript
            [ oneRow [SQLNumber 3]
            , oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW", SQLNumber 2]
            , oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAX"]
            ]
    supersede <- requestGenerationWith supersedeSQL target version offered
    check "wrong superseded identifier rejects replacement" (isUnexpected supersede)
    check "failed supersede never inserts" . (== 3) . length =<< supersedeIssued

    let malformedRevisions =
            [ ("zero", SQLNumber 0)
            , ("negative", SQLNumber (-1))
            , ("fraction", SQLNumber 1.5)
            , ("beyond safe integer", SQLNumber 9007199254740992)
            , ("NaN", SQLNumber (0 / 0))
            , ("infinity", SQLNumber (1 / 0))
            , ("text", SQLText "3")
            , ("null", SQLNull)
            ]
    mapM_
        (\(name, badRevision) -> do
            (execute, _) <- newScript [oneRow [badRevision]]
            result <- requestGenerationWith execute target version offered
            check (name <> " article revision rejected") (isUnexpected result)
        )
        malformedRevisions

    (rowShapeSQL, _) <- newScript [oneRow [SQLNumber 3, SQLNumber 3]]
    badRow <- requestGenerationWith rowShapeSQL target version offered
    check "article revision row shape rejected" (isUnexpected badRow)

reportsClaimAndCompletionFailures :: IO ()
reportsClaimAndCompletionFailures = do
    wanted <- request
    value <- generated
    (claimSQL, _) <- newOutcomeScript [Left sqlFailure]
    claim <- claimGenerationWith claimSQL wanted
    check "claim SQL failure is retryable" (isUnavailable claim)

    let malformed =
            [ ("wrong identifier", oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAX"])
            , ("non-text identifier", oneRow [SQLNumber 3])
            ]
    mapM_
        (\(name, response) -> do
            (execute, _) <- newScript [response]
            result <- claimGenerationWith execute wanted
            check (name <> " claim is rejected") (isUnexpected result)
        )
        malformed

    persisted <- newIORef False
    (failedClaimSQL, _) <- newOutcomeScript [Left sqlFailure]
    failedClaim <- completeGenerationWith failedClaimSQL value $ do
        modifyIORef' persisted (const True)
        pure (Right ())
    check "completion claim SQL failure is retryable" (isUnavailable failedClaim)
    check "failed claim skips article persistence" . not =<< readIORef persisted

    (failedMarkSQL, _) <-
        newOutcomeScript
            [Right (oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]), Left sqlFailure]
    failedMark <- completeGenerationWith failedMarkSQL value (pure (Right ()))
    check "completion update SQL failure is retryable" (isUnavailable failedMark)

    (malformedMarkSQL, _) <-
        newScript
            [ oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]
            , oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAX"]
            ]
    malformedMark <- completeGenerationWith malformedMarkSQL value (pure (Right ()))
    check "completion update cannot return wrong request" (isUnexpected malformedMark)

reportsAbandonFailures :: IO ()
reportsAbandonFailures = do
    wanted <- request
    (failedClaimSQL, _) <- newOutcomeScript [Left sqlFailure]
    failedClaim <- abandonGenerationWith failedClaimSQL wanted
    check "abandon claim SQL failure is retryable" (isUnavailable failedClaim)

    (failedMarkSQL, _) <-
        newOutcomeScript
            [Right (oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]), Left sqlFailure]
    failedMark <- abandonGenerationWith failedMarkSQL wanted
    check "abandon update SQL failure is retryable" (isUnavailable failedMark)

    (lostMarkSQL, _) <-
        newScript [oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"], emptyResult]
    lostMark <- abandonGenerationWith lostMarkSQL wanted
    check "lost abandon update is a transaction failure" (isChanged lostMark)

    (malformedMarkSQL, _) <-
        newScript
            [ oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]
            , oneRow [SQLNumber 3]
            ]
    malformedMark <- abandonGenerationWith malformedMarkSQL wanted
    check "abandon update cannot accept malformed row" (isUnexpected malformedMark)
