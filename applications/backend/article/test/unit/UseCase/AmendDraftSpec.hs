module UseCase.AmendDraftSpec (run) where

import Control.Monad (forM_)
import Data.IORef
import Data.Set qualified as Set
import Domain.Article qualified as Article
import Domain.Article.Common
import Domain.Article.Draft qualified as Draft
import Domain.Article.Private qualified as Private
import Domain.Article.Published qualified as Published
import Shared.Domain.Error (
    DomainError (..),
    createAggregateNotFound,
    createInvariantViolation,
    createOperationNotAllowed,
    createServiceUnavailable,
    createUnexpectedError,
 )
import Shared.Domain.Excerpt (newExcerpt)
import Shared.UseCase.Command qualified as Command
import TestSupport
import UseCase.AmendDraft qualified as Amend
import UseCase.LegacyPersistence
import UseCase.TestSupport
import UseCase.TransactionSupport qualified as Tx

run :: IO ()
run = do
    value <- right identifier
    initial <- right start
    available <- right confirmed
    proof <- right (Draft.proofread (timestamp 1) available initial)
    excerpt <- right (newExcerpt "Summary")
    ready <- right (Draft.prepareToPublish (timestamp 2) excerpt proof)
    published <- right (Published.publish (timestamp 3) ready)
    private <- right (Private.takeDown (timestamp 4) published)
    calls <- newIORef []
    loads <- newIORef []
    let dependencies article outcome =
            Tx.amendDraftDependencies
                ( \requested -> do
                    modifyIORef' loads (<> [requested])
                    pure (Right (Just (LoadedArticle article (recorder calls outcome))))
                )
                extractImages
    request <- command (amendmentPayload value input)
    forM_ [Article.Unvalidated initial, Article.Proofreaded proof, Article.Ready ready] $ \state -> do
        writeIORef calls []
        writeIORef loads []
        result <- Amend.amendDraft (dependencies state (Right ())) request >>= right
        checkPersisted calls request result.article result.events
        check "load requested article once" . (== [value]) =<< readIORef loads
        check
            "identical input still becomes unvalidated and updates time"
            ( Draft.draftIdentifier result.article == value
                && (Draft.draftTimeline result.article).createdAt == timestamp 0
                && (Draft.draftTimeline result.article).updatedAt == request.timestamp
                && draftBodyText (Draft.draftContent result.article).body == input.body
            )
    writeIORef calls []
    clear <- command (amendmentPayload value (DraftInput "New title" "" Nothing []))
    cleared <- Amend.amendDraft (dependencies (Article.Ready ready) (Right ())) clear >>= right
    checkPersisted calls clear cleared.article cleared.events
    check
        "full replacement clears images tags slug body"
        ( Set.null (Draft.draftContent cleared.article).images
            && null (Draft.draftContent cleared.article).tags
            && (Draft.draftContent cleared.article).slug == Nothing
            && draftBodyText (Draft.draftContent cleared.article).body == ""
        )
    forM_ [Article.Published published, Article.Private private] $ \state -> do
        writeIORef calls []
        result <- Amend.amendDraft (dependencies state (Right ())) request
        check "published and private cannot be edited directly" $ case result of
            Left (OperationNotAllowed _) -> True
            _ -> False
        check "forbidden state never saved" . null =<< readIORef calls
    forM_
        [ DraftInput "" "" Nothing []
        , DraftInput "Title" "" (Just "BAD") []
        , DraftInput "Title" "" Nothing [""]
        ]
        $ \invalid -> do
            writeIORef calls []
            invalidRequest <- command (amendmentPayload value invalid)
            result <- Amend.amendDraft (dependencies (Article.Unvalidated initial) (Right ())) invalidRequest
            case newDraftContent extractImages invalid of
                Left err -> expectError "invalid amendment propagates error" err result
                Right _ -> fail "expected invalid fixture"
            check "invalid amendment not saved" . null =<< readIORef calls
    writeIORef calls []
    let extractionError = createInvariantViolation "Images" "bad URL"
        extractFail =
            Tx.amendDraftDependencies
                ( \_ ->
                    pure
                        ( Right
                            ( Just
                                ( LoadedArticle
                                    (Article.Ready ready)
                                    (recorder calls (Right ()))
                                )
                            )
                        )
                )
                (const (Left extractionError))
    extracted <- Amend.amendDraft extractFail request
    expectError "extraction failure" extractionError extracted
    check "extraction failure not saved" . null =<< readIORef calls
    stale <-
        Amend.amendDraft
            (dependencies (Article.Ready ready) (Right ()))
            request{Command.timestamp = timestamp 0}
    check "backwards timestamp rejected" $ case stale of
        Left (InvariantViolation _) -> True
        _ -> False
    check "stale command not saved" . null =<< readIORef calls
    missing <-
        Amend.amendDraft
            ( Tx.amendDraftDependencies
                (const (pure (Right Nothing)))
                extractImages
            )
            request
    expectError
        "missing article"
        ( createAggregateNotFound
            "Article"
            (articleIdentifierText value)
        )
        missing
    let loadError = createServiceUnavailable "ArticleStore" "load failed"
    failedLoad <-
        Amend.amendDraft
            ( Tx.amendDraftDependencies
                (const (pure (Left loadError)))
                extractImages
            )
            request
    expectError "load error" loadError failedLoad
    another <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    wrongRequest <- command (amendmentPayload another input)
    wrong <- Amend.amendDraft (dependencies (Article.Unvalidated initial) (Right ())) wrongRequest
    expectError
        "wrong loaded identity"
        (createUnexpectedError "Article" "loaded identity does not match request")
        wrong
    check "wrong identity not saved" . null =<< readIORef calls
    forM_
        [ createOperationNotAllowed "Slug" "already in use"
        , createOperationNotAllowed "Article" "concurrent update"
        , createServiceUnavailable "ArticleStore" "commit failed"
        ]
        $ \err -> do
            writeIORef calls []
            failed <- Amend.amendDraft (dependencies (Article.Ready ready) (Left err)) request
            expectError "save conflict or failure propagates" err failed
            check "one conditional commit attempt" . (== 1) . length =<< readIORef calls
