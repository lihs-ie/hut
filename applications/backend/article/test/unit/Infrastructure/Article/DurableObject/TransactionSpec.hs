{-# LANGUAGE TypeApplications #-}

module Infrastructure.Article.DurableObject.TransactionSpec (run) where

import Control.Exception
    (AsyncException (UserInterrupt), SomeAsyncException, SomeException, throwIO, try)
import Control.Monad (unless)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Infrastructure.Article.DurableObject.Transaction
    (newArticleTransactionDriverWith, newArticleTransactionDriverWithContext)
import Shared.Domain.Common.Transaction (Transaction, fromEither, runTransaction)
import Shared.Domain.Error
    (DomainError (..), createOperationNotAllowed)
import Shared.Infrastructure.Transaction
    (TransactionOutcome (..), newTransactionManager, transactionAction, withTransaction)

check :: String -> Bool -> IO ()
check label condition = unless condition (fail label)

fakeNative :: IORef Int -> IO a -> IO a
fakeNative stored callback = do
    before <- readIORef stored
    result <- try @SomeException callback
    case result of
        Right value -> pure value
        Left exception -> do
            writeIORef stored before
            throwIO exception

increment :: Transaction (IORef Int) IO Int
increment = transactionAction $ \stored -> do
    modifyIORef' stored (+ 1)
    Right <$> readIORef stored

businessError :: DomainError
businessError = createOperationNotAllowed "Article" "cannot update"

run :: IO Bool
run = do
    stored <- newIORef 0
    nativeCalls <- newIORef (0 :: Int)
    driver <- newArticleTransactionDriverWith
        (\callback -> do
            modifyIORef' nativeCalls (+ 1)
            fakeNative stored callback)
        stored
    let manager = newTransactionManager driver

    committed <- runTransaction manager increment
    check "successful callback commits" (committed == Right 1)
    check "committed value remains" . (== 1) =<< readIORef stored

    rolledBack <- runTransaction manager $ do
        _ <- increment
        fromEither (Left businessError :: Either DomainError Int)
    check "Left preserves the DomainError" (rolledBack == Left businessError)
    check "Left rolls back writes" . (== 1) =<< readIORef stored

    thrownDomain <- runTransaction manager $ transactionAction $ \context -> do
        modifyIORef' context (+ 1)
        throwIO businessError :: IO (Either DomainError Int)
    check "thrown DomainError is typed rollback" (thrownDomain == Left businessError)
    check "thrown DomainError rolls back" . (== 1) =<< readIORef stored

    unexpected <- withTransaction driver $ \context -> do
        modifyIORef' context (+ 1)
        ioError (userError "callback failed") :: IO (Either DomainError ())
    check "callback exception is a confirmed rollback" $ case unexpected of
        RolledBack (UnexpectedError _) -> True
        _ -> False
    check "callback exception rolls back" . (== 1) =<< readIORef stored

    unknownDriver <- newArticleTransactionDriverWith
            (\callback -> do
                _ <- callback
                ioError (userError "commit acknowledgement lost"))
            stored
    unknown <- withTransaction unknownDriver $ \context -> do
        modifyIORef' context (+ 1)
        pure (Right ())
    check "unconfirmed native outcome is unknown" $ case unknown of
        OutcomeUnknown _ -> True
        _ -> False
    check "unknown does not retry callback" . (== 2) =<< readIORef stored

    beforeNested <- readIORef nativeCalls

    nested <- withTransaction driver $ \_ -> do
        inner <- withTransaction driver $ \_ -> pure (Right ())
        pure $ case inner of
            RolledBack err -> Left err
            _ -> Right ()
    check "nested transaction is rejected" $ case nested of
        RolledBack (OperationNotAllowed _) -> True
        _ -> False
    afterNested <- readIORef nativeCalls
    check "nested call never enters native transaction" (afterNested == beforeNested + 1)

    nativeDomainDriver <- newArticleTransactionDriverWith
        (\_ -> throwIO businessError)
        stored
    nativeDomain <- withTransaction nativeDomainDriver $ \_ -> pure (Right ())
    check "native DomainError does not imply rollback" $ case nativeDomain of
        OutcomeUnknown _ -> True
        _ -> False

    cancelled <- try @SomeAsyncException $ withTransaction driver $ \context -> do
        modifyIORef' context (+ 1)
        throwIO UserInterrupt :: IO (Either DomainError ())
    check "cancellation propagates" $ case cancelled of
        Left _ -> True
        Right _ -> False
    check "cancellation rolls back" . (== 2) =<< readIORef stored

    afterCancellation <- runTransaction manager increment
    check "driver can run after cancellation" (afterCancellation == Right 3)

    createdContexts <- newIORef (0 :: Int)
    freshDriver <- newArticleTransactionDriverWithContext id $
        atomicModifyIORef' createdContexts $ \value -> (value + 1, value + 1)
    firstContext <- withTransaction freshDriver (pure . Right)
    secondContext <- withTransaction freshDriver (pure . Right)
    check "transaction context is created anew on every entry"
        (case (firstContext, secondContext) of
            (Committed 1, Committed 2) -> True
            _ -> False)
    pure True
