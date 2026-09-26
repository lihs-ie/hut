module UseCase.JotDownSpec (run) where

import Control.Monad (forM_)
import Data.IORef
import Data.Set qualified as Set
import Domain.Article.Common
import Domain.Article.Draft
import Shared.Domain.Error (createInvariantViolation, createOperationNotAllowed, createServiceUnavailable)
import Shared.UseCase.Command qualified
import TestSupport
import UseCase.JotDown qualified as Jot
import UseCase.TestSupport
import UseCase.TransactionSupport qualified as Tx

run :: IO ()
run = do
    value <- right identifier
    calls <- newIORef []
    generations <- newIORef (0 :: Int)
    let generate = modifyIORef' generations (+ 1) >> pure (Right value)
        dependencies = Tx.jotDownDependencies generate extractImages (recorder calls (Right ()))
    request <- command input
    result <- Jot.jotDown dependencies request >>= right
    checkPersisted calls request result.article result.events
    check "identifier generated once" . (== 1) =<< readIORef generations
    check
        "command time used for creation and update"
        ( (draftTimeline result.article).createdAt == request.timestamp
            && (draftTimeline result.article).updatedAt == request.timestamp
        )
    writeIORef calls []
    titleOnly <- command (DraftInput "Idea" "" Nothing [])
    minimal <- Jot.jotDown dependencies titleOnly >>= right
    checkPersisted calls titleOnly minimal.article minimal.events
    check
        "title only permitted"
        ( (draftContent minimal.article).slug == Nothing
            && draftBodyText (draftContent minimal.article).body == ""
            && Set.null (draftContent minimal.article).images
        )
    forM_
        [ DraftInput "" "" Nothing []
        , DraftInput "Title" "" (Just "BAD") []
        , DraftInput "Title" "" Nothing [""]
        ]
        $ \invalid -> do
            writeIORef calls []
            writeIORef generations 0
            invalidCommand <- command invalid
            actual <- Jot.jotDown dependencies invalidCommand
            case newDraftContent extractImages invalid of
                Left err -> expectError "validation error propagated" err actual
                Right _ -> fail "expected invalid fixture"
            check "invalid input is not saved" . null =<< readIORef calls
            check "validation runs before identifier generation" . (== 0) =<< readIORef generations
    let generationError = createServiceUnavailable "Identifier" "entropy unavailable"
        extractionError = createInvariantViolation "Images" "invalid managed URL"
        storageError = createServiceUnavailable "ArticleStore" "unavailable"
        conflictError = createOperationNotAllowed "Slug" "already in use"
    writeIORef calls []
    failed <-
        Jot.jotDown
            ( Tx.jotDownDependencies
                (pure (Left generationError))
                extractImages
                (recorder calls (Right ()))
            )
            request
    expectError "identifier error propagated" generationError failed
    check "no save on identifier failure" . null =<< readIORef calls
    writeIORef generations 0
    failedExtraction <-
        Jot.jotDown
            ( Tx.jotDownDependencies
                generate
                (const (Left extractionError))
                (recorder calls (Right ()))
            )
            request
    expectError "extraction error propagated" extractionError failedExtraction
    check "no generation on extraction failure" . (== 0) =<< readIORef generations
    check "no save on extraction failure" . null =<< readIORef calls
    forM_ [storageError, conflictError] $ \err -> do
        writeIORef calls []
        failedPersist <-
            Jot.jotDown
                ( Tx.jotDownDependencies
                    generate
                    extractImages
                    (recorder calls (Left err))
                )
                request
        expectError "failed commit never returns a successful result" err failedPersist
        check "one commit attempt" . (== 1) . length =<< readIORef calls
