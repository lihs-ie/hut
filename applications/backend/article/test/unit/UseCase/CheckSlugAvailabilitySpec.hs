module UseCase.CheckSlugAvailabilitySpec (run) where

import Control.Monad (forM_)
import Domain.Article.Common
import Shared.Domain.Error (createAggregateNotFound, createUnexpectedError)
import Shared.Domain.Slug (slugText)
import TestSupport
import UseCase.CheckSlugAvailability qualified as Slug
import UseCase.ReadingSupport
import UseCase.TestSupport (command, expectError)

run :: IO ()
run = do
    value <- right identifier
    other <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    articles <- states
    request <- command (Slug.CheckSlugAvailabilityPayload value "haskell-syntax")
    let dependencies found owner =
            Slug.Dependencies
                (\requested -> check "target identity" (requested == value) >> pure found)
                (\slug -> check "candidate validated" (slugText slug == "haskell-syntax") >> pure owner)
    forM_ articles $ \article ->
        forM_ [(Nothing, Slug.Available), (Just value, Slug.Available), (Just other, Slug.InUse)] $ \(owner, expected) -> do
            result <- Slug.checkSlugAvailability (dependencies (Right (Just article)) (Right owner)) request >>= right
            check "owner comparison" (result.availability == expected)
            checkEmptyEvents result.events
    let noOwner found = Slug.Dependencies (const (pure found)) (const (fail "unexpected owner lookup"))
    missing <- Slug.checkSlugAvailability (noOwner (Right Nothing)) request
    expectError "missing target" (createAggregateNotFound "Article" (articleIdentifierText value)) missing
    failed <- Slug.checkSlugAvailability (noOwner (Left failure)) request
    expectError "target load error" failure failed
    forM_ articles $ \article -> do
        failedOwner <- Slug.checkSlugAvailability (dependencies (Right (Just article)) (Left failure)) request
        expectError "owner load error" failure failedOwner
        wrong <- command (Slug.CheckSlugAvailabilityPayload other "haskell-syntax")
        mismatch <- Slug.checkSlugAvailability (noOwner (Right (Just article))) wrong
        expectError
            "wrong target identity"
            (createUnexpectedError "Article" "loaded identity does not match request")
            mismatch
    invalid <- command (Slug.CheckSlugAvailabilityPayload value "INVALID")
    invalidResult <-
        Slug.checkSlugAvailability
            (Slug.Dependencies (const (fail "invalid input queried")) (const (fail "invalid slug queried")))
            invalid
    checkFailure invalidResult
