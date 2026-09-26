module Infrastructure.Article.DurableObject.CompletionSpec (run) where

import Cloudflare.Workers.Binding.DurableObject.SQL (
    SQLResult (..),
    SQLStatement (..),
    SQLValue (..),
 )
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as Text
import Domain.Article (Article (..))
import Domain.Article.Draft (proofread)
import Infrastructure.Article.DurableObject.Completion (applyGeneratedExcerptWith)
import Infrastructure.Article.DurableObject.GenerationJob (GenerationFinalization (..))
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerated (..),
    ExcerptGeneratedMessage (..),
    newGenerationRequestIdentifier,
 )
import Shared.Domain.Common.Primitive (newPositiveInteger)
import Shared.Domain.Error (DomainError)
import Shared.Domain.Excerpt (newExcerpt)
import Shared.Infrastructure.Transaction (
    TransactionDriver (..),
    TransactionOutcome (..),
    newTransactionManager,
    transactionAction,
 )
import Shared.Infrastructure.Versioning (newVersion)
import Shared.UseCase.Command (
    Command (..),
    actorText,
    causationText,
    correlationIdentifierText,
    newActor,
    newCorrelationIdentifier,
 )
import Shared.UseCase.Event (newEventEnvelope, newEventIdentifier)
import TestSupport (check, confirmed, identifier, right, start)
import TestSupport qualified as Support
import UseCase.PrepareToPublish qualified as Prepare

requestText :: Text.Text
requestText = "01ARZ3NDEKTSV4RRFFQ69G5FAW"

row :: SQLResult
row = SQLResult [] [SQLText requestText : []] 0 1

empty :: SQLResult
empty = SQLResult [] [] 0 0

message :: IO ExcerptGeneratedMessage
message = do
    event <- right (newEventIdentifier "completion-event")
    request <- right (newGenerationRequestIdentifier requestText)
    article <- right identifier
    revision <- newVersion <$> right (newPositiveInteger 3)
    excerpt <- right (newExcerpt "Generated excerpt")
    actor <- right (newActor "system")
    correlation <- right (newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAX")
    pure $ ExcerptGeneratedMessage $
        newEventEnvelope event (Support.timestamp 2) actor correlation Nothing
            (ExcerptGenerated request article revision excerpt)

driver :: TransactionDriver () IO
driver = TransactionDriver $ \action -> do
    result <- action ()
    pure $ either RolledBack Committed result

run :: IO ()
run = do
    appliesWithoutNestedTransaction
    acknowledgesObsoleteWithoutWriting
    rollsBackFailedTransition
    reportsUnknownTransactionOutcome

appliesWithoutNestedTransaction :: IO ()
appliesWithoutNestedTransaction = do
    generated <- message
    original <- right start
    images <- right confirmed
    draft <- right (proofread (Support.timestamp 1) images original)
    saved <- newIORef Nothing
    eventCount <- newIORef (0 :: Int)
    sqlCalls <- newIORef (0 :: Int)
    let execute _ = \statement -> do
            calls <- readIORef sqlCalls
            writeIORef sqlCalls (calls + 1)
            check "completion SQL statement is for the job" $
                "article_generation_jobs" `Text.isInfixOf` statement.sql
            pure row
        dependencies = Prepare.Dependencies
            { Prepare.transactionManager = newTransactionManager driver
            , Prepare.findArticle = \_ -> transactionAction $ \_ ->
                pure (Right (Just (Proofreaded draft)))
            , Prepare.persistArticle = \article -> transactionAction $ \_ -> do
                writeIORef saved (Just article)
                pure (Right ())
            , Prepare.appendEvents = \command _ -> transactionAction $ \_ -> do
                check "completion forwards event context"
                    (actorText command.actor == "system"
                        && correlationIdentifierText command.correlation
                            == "01ARZ3NDEKTSV4RRFFQ69G5FAX"
                        && fmap causationText command.causation
                            == Just "completion-event")
                writeIORef eventCount 1
                pure (Right ())
            }
    result <- applyGeneratedExcerptWith driver execute dependencies generated
    check "completion finalizes the active job" $
        result == Right GenerationFinalized
    check "completion persisted a ready article" . maybe False isReady =<< readIORef saved
    check "completion appended the ready event" . (== 1) =<< readIORef eventCount
    check "completion queried and marked the job" . (== 2) =<< readIORef sqlCalls
  where
    isReady Ready{} = True
    isReady _ = False

acknowledgesObsoleteWithoutWriting :: IO ()
acknowledgesObsoleteWithoutWriting = do
    generated <- message
    called <- newIORef False
    let dependencies = Prepare.Dependencies
            { Prepare.transactionManager = newTransactionManager driver
            , Prepare.findArticle = \_ -> transactionAction $ \_ -> do
                writeIORef called True
                pure (Right Nothing)
            , Prepare.persistArticle = \_ -> transactionAction $ \_ -> do
                writeIORef called True
                pure (Right ())
            , Prepare.appendEvents = \_ _ -> transactionAction $ \_ -> do
                writeIORef called True
                pure (Right ())
            }
    result <- applyGeneratedExcerptWith driver (\_ _ -> pure empty) dependencies generated
    check "obsolete job is terminal" $
        result == Right GenerationFinalizationTerminalAck
    check "obsolete job skips the use case" . not =<< readIORef called

rollsBackFailedTransition :: IO ()
rollsBackFailedTransition = do
    generated <- message
    original <- right start
    sqlCalls <- newIORef (0 :: Int)
    let execute _ _ = do
            calls <- readIORef sqlCalls
            writeIORef sqlCalls (calls + 1)
            pure row
        dependencies = Prepare.Dependencies
            { Prepare.transactionManager = newTransactionManager driver
            , Prepare.findArticle = \_ -> transactionAction $ \_ ->
                pure (Right (Just (Unvalidated original)))
            , Prepare.persistArticle = \_ -> transactionAction $ \_ ->
                fail "invalid transition must not persist"
            , Prepare.appendEvents = \_ _ -> transactionAction $ \_ ->
                fail "invalid transition must not append"
            }
    result <- applyGeneratedExcerptWith driver execute dependencies generated
    check "invalid transition rolls back" $ case result of
        Left (_ :: DomainError) -> True
        Right _ -> False
    check "failed transition never marks the job completed" . (== 1) =<< readIORef sqlCalls

reportsUnknownTransactionOutcome :: IO ()
reportsUnknownTransactionOutcome = do
    generated <- message
    let unknownDriver = TransactionDriver $ \_ -> pure (OutcomeUnknown "commit uncertain")
        dependencies = Prepare.Dependencies
            { Prepare.transactionManager = newTransactionManager unknownDriver
            , Prepare.findArticle = \_ -> transactionAction $ \_ ->
                fail "unknown transaction must not run the use case"
            , Prepare.persistArticle = \_ -> transactionAction $ \_ ->
                fail "unknown transaction must not persist"
            , Prepare.appendEvents = \_ _ -> transactionAction $ \_ ->
                fail "unknown transaction must not append"
            }
    outcome <- applyGeneratedExcerptWith unknownDriver
        (\_ _ -> fail "unknown transaction must not query") dependencies generated
    check "unknown transaction maps to domain error" (case outcome of
        Left err -> "TransactionOutcomeUnknown" `Text.isInfixOf` Text.pack (show err)
        _ -> False)
