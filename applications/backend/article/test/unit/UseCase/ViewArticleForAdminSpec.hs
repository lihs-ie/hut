module UseCase.ViewArticleForAdminSpec (run) where

import Control.Monad (forM_)
import Domain.Article.Common
import Shared.Domain.Error (createAggregateNotFound, createUnexpectedError)
import TestSupport
import UseCase.ReadingSupport
import UseCase.TestSupport (command, expectError)
import UseCase.TransactionSupport qualified as Tx
import UseCase.ViewArticleForAdmin qualified as View

run :: IO ()
run = do
    value <- right identifier
    request <- command (View.ViewArticleForAdminPayload value)
    articles <- states
    forM_ articles $ \article -> do
        result <-
            View.viewArticleForAdmin
                (Tx.viewArticleForAdminDependencies (\requested -> check "requested identity" (requested == value) >> pure (Right (Just article))))
                request
                >>= right
        check "full article in every state" (result.article == article)
        checkEmptyEvents result.events
    missing <- View.viewArticleForAdmin (Tx.viewArticleForAdminDependencies (const (pure (Right Nothing)))) request
    expectError "not found" (createAggregateNotFound "Article" (articleIdentifierText value)) missing
    failed <- View.viewArticleForAdmin (Tx.viewArticleForAdminDependencies (const (pure (Left failure)))) request
    expectError "failure preserved" failure failed
    other <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    wrong <- command (View.ViewArticleForAdminPayload other)
    forM_ articles $ \article -> do
        result <- View.viewArticleForAdmin (Tx.viewArticleForAdminDependencies (const (pure (Right (Just article))))) wrong
        expectError "identity mismatch" (createUnexpectedError "Article" "loaded identity does not match request") result
