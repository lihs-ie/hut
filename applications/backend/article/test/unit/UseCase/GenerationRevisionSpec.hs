{-# LANGUAGE GADTs #-}

module UseCase.GenerationRevisionSpec (run) where

import Data.IORef
import Domain.Article (Article (..), articleIdentifier)
import Domain.Article.Common
import Domain.Article.Draft qualified as Draft
import Shared.Domain.Error (createOperationNotAllowed)
import Shared.Domain.Event (Events (..))
import Shared.Domain.Excerpt (excerptText)
import TestSupport
import UseCase.Persistence
import UseCase.PrepareToPublish qualified as Prepare
import UseCase.TestSupport (command, expectError)

-- This models the required adapter CAS contract; it does not test D1 itself.
data Store = Store Int Article Int

run :: IO ()
run = do
    value <- right identifier
    initial <- right start
    available <- right confirmed
    proof <- right (Draft.proofread (timestamp 1) available initial)
    store <- newIORef (Store 1 (Proofreaded proof) 0)
    let stale = createOperationNotAllowed "Article" "stale generation revision"
        loadAt expected beforeCommit requested = do
            Store actual article _ <- readIORef store
            if articleIdentifier article /= requested
                then pure (Right Nothing)
                else
                    if actual /= expected
                        then pure (Left stale)
                        else
                            pure
                                ( Right
                                    ( Just
                                        ( LoadedForPreparation
                                            article
                                            ( \_ prepared (Events events) -> do
                                                beforeCommit
                                                atomicModifyIORef' store $ \current@(Store revision currentArticle count) ->
                                                    if revision /= expected || articleIdentifier currentArticle /= requested
                                                        then (current, Left stale)
                                                        else
                                                            ( Store
                                                                (revision + 1)
                                                                (Ready prepared)
                                                                (count + length events)
                                                            , Right ()
                                                            )
                                            )
                                        )
                                    )
                                )
        dependencies beforeCommit =
            Prepare.Dependencies
                (loadAt 1 beforeCommit)
                ( \requested -> do
                    Store revision _ _ <- readIORef store
                    loadAt revision (pure ()) requested
                )
        checkStore name expectedRevision expectedArticle expectedEvents = do
            Store revision article count <- readIORef store
            check
                name
                ( revision == expectedRevision
                    && article == expectedArticle
                    && count == expectedEvents
                )
    generated <- command (Prepare.ApplyGeneratedExcerpt value "Generated")

    -- Even if editing and re-proofreading return to the same state, the old
    -- generation's target revision must not be replaced with the current one.
    writeIORef store (Store 2 (Proofreaded proof) 0)
    outdated <- Prepare.prepareToPublish (dependencies (pure ())) generated
    expectError "older generation rejected at load" stale outdated
    checkStore "outdated result leaves article and outbox unchanged" 2 (Proofreaded proof) 0

    writeIORef store (Store 1 (Proofreaded proof) 0)
    conflicted <-
        Prepare.prepareToPublish
            (dependencies (writeIORef store (Store 2 (Unvalidated initial) 0)))
            generated
    expectError "edit between load and save rejected" stale conflicted
    checkStore "concurrent edit is preserved without outbox append" 2 (Unvalidated initial) 0

    writeIORef store (Store 1 (Proofreaded proof) 0)
    prepared <- Prepare.prepareToPublish (dependencies (pure ())) generated >>= right
    checkStore "first result commits article and one event" 2 (Ready prepared.article) 1
    duplicate <- Prepare.prepareToPublish (dependencies (pure ())) generated
    expectError "duplicate generation rejected" stale duplicate
    checkStore "duplicate does not append another event" 2 (Ready prepared.article) 1

    revision <- command (Prepare.ReviseExcerpt value "Hand edited")
    edited <- Prepare.prepareToPublish (dependencies (pure ())) revision >>= right
    checkStore "manual edit does not append event" 3 (Ready edited.article) 1
    late <- Prepare.prepareToPublish (dependencies (pure ())) generated
    expectError "late generation rejected after manual edit" stale late
    checkStore "manual edit preserved" 3 (Ready edited.article) 1
    check
        "manual text unchanged"
        (excerptText (Draft.publicationContent edited.article).excerpt == "Hand edited")
