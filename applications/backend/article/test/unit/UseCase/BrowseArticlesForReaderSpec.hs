module UseCase.BrowseArticlesForReaderSpec (run) where

import Control.Monad (forM_)

import Shared.Domain.Pager qualified as Pager
import Shared.Domain.Tag (newTagIdentifier)
import TestSupport
import UseCase.BrowseArticlesForReader qualified as Browse
import UseCase.Reading
import UseCase.ReadingSupport
import UseCase.TestSupport (command, expectError)
import UseCase.TransactionSupport qualified as Tx

run :: IO ()
run = do
    article <- published
    let payload current items = Browse.BrowseArticlesForReaderPayload current items Nothing []
        dependencies output =
            Tx.browseArticlesForReaderDependencies
                ( \request -> do
                    check "request forwarded" (pageNumber request == 1 && pageSize request == 10 && pageOffset request == 0)
                    pure output
                )
    request <- command (payload 1 Nothing)
    result <- Browse.browseArticlesForReader (dependencies (Right (1, [article]))) request >>= right
    check "full article returned" (result.articles == [article])
    check "pager metadata" (Pager.total result.pager == 1 && Pager.items result.pager == 10 && Pager.current result.pager == 1)
    checkEmptyEvents result.events
    tag <- right (newTagIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAY")
    filtered <- command (Browse.BrowseArticlesForReaderPayload 1 Nothing (Just "syntax") [tag])
    _ <- Browse.browseArticlesForReader
        (Tx.browseArticlesForReaderDependencies (\criteria -> do
            check "reader search conditions forwarded"
                (keyword criteria == Just "syntax" && tags criteria == [tag])
            pure (Right (1, [article]))))
        filtered >>= right
    failed <- Browse.browseArticlesForReader (dependencies (Left failure)) request
    expectError "read failure" failure failed
    badCount <- Browse.browseArticlesForReader (dependencies (Right (1, []))) request
    checkFailure badCount
    forM_ [(0, Nothing), (1, Just 0), (1, Just 101), (maxBound, Just 100)] $ \(current, items) -> do
        invalid <- command (payload current items)
        outcome <- Browse.browseArticlesForReader (Tx.browseArticlesForReaderDependencies (\_ -> fail "invalid request queried")) invalid
        checkFailure outcome
    forM_ [(0, 1), (1, 3)] $ \(total, current) -> do
        beyond <- command (payload current (Just 1))
        empty <-
            Browse.browseArticlesForReader
                ( Tx.browseArticlesForReaderDependencies
                    ( \page -> do
                        check "paging forwarded" (pageSize page == 1 && pageNumber page == current && pageOffset page == current - 1)
                        pure (Right (total, []))
                    )
                )
                beyond
                >>= right
        check "empty page and total preserved" (null empty.articles && Pager.total empty.pager == total && Pager.current empty.pager == current)
