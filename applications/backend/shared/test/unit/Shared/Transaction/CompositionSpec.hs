module Shared.Transaction.CompositionSpec (run) where

import Shared.Domain.Common.Transaction
import Shared.Transaction.Support

run :: IO Bool
run = do
    fixture <- newFixture Normal
    result <- runTransaction fixture.manager program
    check "result after commit" (result == Right 1)
    check "article and outbox committed" . (== Store (Just 1) [1]) =<< fixture.stored
    check "single boundary and operation ordering"
        . (== ["begin", "read", "persist", "outbox", "commit"])
        =<< fixture.trace

    mapped <- newFixture Normal
    mapping <- runTransaction mapped.manager ((+ 1) <$> pure (4 :: Int))
    check "Functor" (mapping == Right 5)
    applied <- runTransaction mapped.manager (pure (+ 2) <*> fromEither (Right (3 :: Int)))
    check "Applicative" (applied == Right 5)
    pureResult <- runTransaction mapped.manager (pure (7 :: Int) >>= (pure . (+ 1)))
    check "pure bind" (pureResult == Right 8)

    readOnly <- newFixture Normal
    value <- runTransaction readOnly.manager (findArticle)
    check "read only" (value == Right (Just 0))
    check "read does not write" . (== Store (Just 0) []) =<< readOnly.stored

    dependent <- newFixture Normal
    observed <- runTransaction dependent.manager $ do
        persistArticle 20
        findArticle
    check "read own write" (observed == Right (Just 20))

    effectful <- newFixture Normal
    combined <-
        runTransaction
            effectful.manager
            ((\_ value -> value) <$> persistArticle 3 <*> findArticle)
    check "Applicative preserves effects" (combined == Right (Just 3))
    check "Applicative ordering" . (== ["begin", "persist", "read", "commit"]) =<< effectful.trace
    pure True
