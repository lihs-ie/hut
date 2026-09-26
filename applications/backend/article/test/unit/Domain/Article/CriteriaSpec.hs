module Domain.Article.CriteriaSpec (run) where

import Control.Monad (forM_)
import Domain.Article.Criteria
import Shared.Domain.Error (createInvariantViolation)
import TestSupport

run :: IO ()
run = do
    forM_ [AllArticles, UnvalidatedOnly, ProofreadedOnly, ReadyOnly, PublishedOnly, PrivateOnly] $ \selection -> do
        criteria <- right (newCriteria selection 2 (Just 20))
        check
            "selection and paging are one value"
            ( status criteria == selection
                && pageNumber criteria == 2
                && pageSize criteria == 20
                && pageOffset criteria == 20
            )
        same <- right (newCriteria selection 2 (Just 20))
        check "value equality" (criteria == same)
    defaults <- right (newCriteria AllArticles 1 Nothing)
    explicit <- right (newCriteria AllArticles 1 (Just 10))
    check "default normalizes to same value" (defaults == explicit)
    check
        "over-limit size reports its domain rule"
        ( newCriteria AllArticles 1 (Just 101)
            == Left (createInvariantViolation "Pagination" "at most 100 articles per page")
        )
