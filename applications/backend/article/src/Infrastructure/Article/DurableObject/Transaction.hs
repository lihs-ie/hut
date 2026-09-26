{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Infrastructure.Article.DurableObject.Transaction (
    ArticleTransactionContext (..),
    newArticleTransactionDriver,
    newArticleTransactionDriverWith,
    newArticleTransactionDriverWithContext,
) where

import Cloudflare.Workers.Binding.DurableObject
    (DurableObjectStorage, doStorageTransactionWith)
import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception
    (Exception, SomeAsyncException, SomeException, evaluate, finally, fromException, mask, throwIO, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Maybe (isJust)
import "article" Domain.Article (ArticleIdentifier, articleIdentifierText)
import Shared.Domain.Error
    (DomainError, createOperationNotAllowed, createUnexpectedError)
import Shared.Infrastructure.Transaction
    (TransactionDriver (..), TransactionOutcome (..))
import Shared.Infrastructure.Versioning (VersionContext, emptyVersionContext)

newtype AbortTransaction = AbortTransaction DomainError
    deriving stock (Show)

instance Exception AbortTransaction

newtype CallbackFailure = CallbackFailure SomeException
    deriving stock (Show)

instance Exception CallbackFailure

-- The same storage is passed to every operation in the callback. SQL adapters
-- must use sqlExec on it, not sqlExecute's separate transactionSync boundary.
data ArticleTransactionContext = ArticleTransactionContext
    { storage :: DurableObjectStorage
    , versions :: IORef (VersionContext ArticleIdentifier)
    }

newArticleTransactionDriver ::
    DurableObjectStorage -> IO (TransactionDriver ArticleTransactionContext IO)
newArticleTransactionDriver storage =
    newArticleTransactionDriverWithContext
        (doStorageTransactionWith storage)
        (ArticleTransactionContext storage <$> newIORef (emptyVersionContext "Article" articleIdentifierText))

-- The injected runner must rethrow callback exceptions only after rollback is
-- confirmed. The production runner is doStorageTransactionWith.
newArticleTransactionDriverWith ::
    forall context.
    (forall a. IO a -> IO a) -> context -> IO (TransactionDriver context IO)
newArticleTransactionDriverWith runNative context =
    newArticleTransactionDriverWithContext runNative (pure context)

newArticleTransactionDriverWithContext ::
    forall context.
    (forall a. IO a -> IO a) -> IO context -> IO (TransactionDriver context IO)
newArticleTransactionDriverWithContext runNative newContext = do
    activeThreads <- newIORef ([] :: [ThreadId])
    pure $ TransactionDriver $ \action -> mask $ \restore -> do
        thread <- myThreadId
        entered <- atomicModifyIORef' activeThreads $ \threads ->
            if thread `elem` threads
                then (threads, False)
                else (thread : threads, True)
        if entered
            then restore (runOnce action) `finally`
                atomicModifyIORef' activeThreads
                    (\threads -> (filter (/= thread) threads, ()))
            else pure $ RolledBack $
                createOperationNotAllowed
                    "ArticleTransaction"
                    "nested transaction is not allowed"
  where
    runOnce :: forall a. (context -> IO (Either DomainError a)) -> IO (TransactionOutcome a)
    runOnce action = do
        result <- try @SomeException $ runNative $ do
            attempted <- try @SomeException $ do
                context <- newContext
                outcome <- action context
                either (throwIO . AbortTransaction) evaluate outcome
            case attempted of
                Right value -> pure value
                Left exception
                    | isJust (fromException @AbortTransaction exception) ->
                        throwIO exception
                    | Just domainError <- fromException @DomainError exception ->
                        throwIO (AbortTransaction domainError)
                    | otherwise -> throwIO (CallbackFailure exception)
        case result of
            Right value -> pure (Committed value)
            Left exception
                | Just (AbortTransaction domainError) <- fromException exception ->
                    pure (RolledBack domainError)
                | Just (CallbackFailure callbackException) <- fromException exception ->
                    if isJust (fromException @SomeAsyncException callbackException)
                        then throwIO callbackException
                        else pure $ RolledBack $
                            createUnexpectedError
                                "ArticleTransaction"
                                "transaction callback raised an unexpected exception"
                | isJust (fromException @SomeAsyncException exception) ->
                    throwIO exception
                | otherwise ->
                    pure (OutcomeUnknown "native transaction outcome was not confirmed")
