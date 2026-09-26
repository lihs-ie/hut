{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

module Shared.Domain.Common.Transaction.Internal (
    Transaction (..),
    TransactionManager (..),
) where

import Shared.Domain.Error (DomainError)

-- Internal only: every bind supplies the SAME execution context.
type role Transaction nominal nominal nominal
newtype Transaction context m a = Transaction (context -> m (Either DomainError a))

instance (Monad m) => Functor (Transaction context m) where
    fmap f action = action >>= (pure . f)

instance (Monad m) => Applicative (Transaction context m) where
    pure value = Transaction (\_ -> pure (Right value))
    functions <*> values = do
        f <- functions
        value <- values
        pure (f value)

instance (Monad m) => Monad (Transaction context m) where
    Transaction action >>= next = Transaction $ \context -> do
        outcome <- action context
        case outcome of
            Left err -> pure (Left err)
            Right value -> case next value of
                Transaction continuation -> continuation context

type role TransactionManager nominal nominal
newtype TransactionManager context m
    = TransactionManager
        (forall a. Transaction context m a -> m (Either DomainError a))
