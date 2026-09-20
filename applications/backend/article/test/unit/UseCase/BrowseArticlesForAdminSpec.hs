module UseCase.BrowseArticlesForAdminSpec (run) where

import Control.Monad (forM_)
import Domain.Article (Article (..))
import Shared.Domain.Pager qualified as Pager
import TestSupport
import UseCase.BrowseArticlesForAdmin qualified as Browse
import UseCase.Reading hiding (status)
import UseCase.ReadingSupport
import UseCase.TestSupport (command, expectError)
import UseCase.TransactionSupport qualified as Tx

run :: IO ()
run = do
    article <- Unvalidated <$> right start
    let payload current items = Browse.BrowseArticlesForAdminPayload current items AllArticles
        dependencies output =
            Tx.browseArticlesForAdminDependencies
                ( \status request -> do
                    check "filter forwarded" (status == AllArticles)
                    check "request forwarded" (pageNumber request == 1 && pageSize request == 10 && pageOffset request == 0)
                    pure output
                )
    request <- command (payload 1 Nothing)
    result <- Browse.browseArticlesForAdmin (dependencies (Right (1, [article]))) request >>= right
    check "full article returned" (result.articles == [article])
    check "pager metadata" (Pager.total result.pager == 1 && Pager.items result.pager == 10 && Pager.current result.pager == 1)
    checkEmptyEvents result.events
    failed <- Browse.browseArticlesForAdmin (dependencies (Left failure)) request
    expectError "read failure" failure failed
    badCount <- Browse.browseArticlesForAdmin (dependencies (Right (1, []))) request
    checkFailure badCount
    forM_ [(0, Nothing), (1, Just 0), (1, Just 101), (maxBound, Just 100)] $ \(current, items) -> do
        invalid <- command (payload current items)
        outcome <- Browse.browseArticlesForAdmin (Tx.browseArticlesForAdminDependencies (\_ _ -> fail "invalid request queried")) invalid
        checkFailure outcome
    forM_ [(0, 1), (1, 3)] $ \(total, current) -> do
        beyond <- command (payload current (Just 1))
        empty <-
            Browse.browseArticlesForAdmin
                ( Tx.browseArticlesForAdminDependencies
                    ( \_ page -> do
                        check "paging forwarded" (pageSize page == 1 && pageNumber page == current && pageOffset page == current - 1)
                        pure (Right (total, []))
                    )
                )
                beyond
                >>= right
        check "empty page and total preserved" (null empty.articles && Pager.total empty.pager == total && Pager.current empty.pager == current)
    articles <- states
    forM_ [UnvalidatedOnly, ProofreadedOnly, ReadyOnly, PublishedOnly, PrivateOnly] $ \status -> do
        filteredRequest <- command (Browse.BrowseArticlesForAdminPayload 1 Nothing status)
        let selected = filter (matchesFilter status) articles
        filtered <-
            Browse.browseArticlesForAdmin
                (Tx.browseArticlesForAdminDependencies (\actual _ -> check "specific filter forwarded" (actual == status) >> pure (Right (length selected, selected))))
                filteredRequest
                >>= right
        check "filtered articles intact" (filtered.articles == selected)
    privateRequest <- command (Browse.BrowseArticlesForAdminPayload 1 Nothing PrivateOnly)
    mismatch <-
        Browse.browseArticlesForAdmin
            (Tx.browseArticlesForAdminDependencies (\_ _ -> pure (Right (1, [article]))))
            privateRequest
    checkFailure mismatch
