module UseCase.ReadArticleSpec (run) where

import Control.Monad (forM_)
import Domain.Article (Article (..))
import Shared.Domain.Error (createAggregateNotFound, createUnexpectedError)
import Shared.Domain.Slug (slugText)
import TestSupport
import UseCase.ReadArticle qualified as Read
import UseCase.ReadingSupport
import UseCase.TestSupport (command, expectError)

run :: IO ()
run = do
    article <- published
    request <- command (Read.ReadArticlePayload "haskell-syntax")
    let dependencies found =
            Read.Dependencies
                (\slug -> check "validated slug" (slugText slug == "haskell-syntax") >> pure found)
    result <- Read.readArticle (dependencies (Right (Just (Published article)))) request >>= right
    check "published article returned intact" (result.article == article)
    checkEmptyEvents result.events
    articles <- states
    forM_ (Nothing : [Just value | value <- articles, case value of Published _ -> False; _ -> True]) $ \found -> do
        outcome <- Read.readArticle (dependencies (Right found)) request
        expectError
            "hidden and absent are indistinguishable"
            (createAggregateNotFound "Article" "haskell-syntax")
            outcome
    failed <- Read.readArticle (dependencies (Left failure)) request
    expectError "failure preserved" failure failed
    forM_ ["", "Invalid", "-invalid", "invalid--slug"] $ \slug -> do
        invalid <- command (Read.ReadArticlePayload slug)
        outcome <- Read.readArticle (Read.Dependencies (const (fail "invalid input queried"))) invalid
        checkFailure outcome
    different <- command (Read.ReadArticlePayload "different")
    mismatch <- Read.readArticle (Read.Dependencies (const (pure (Right (Just (Published article)))))) different
    expectError
        "wrong slug not returned"
        (createUnexpectedError "Article" "loaded slug does not match request")
        mismatch
