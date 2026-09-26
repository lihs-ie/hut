module Domain.Article.CriteriaSpec (run) where

import Control.Monad (forM_)
import Data.Text qualified as Text
import Domain.Article.Criteria
import Shared.Domain.Error (createInvariantViolation)
import Shared.Domain.Tag (newTagIdentifier)
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
    tag <- right (newTagIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAY")
    reader <- right (newReaderCriteria 1 (Just 20) (Just "syntax") [tag])
    check "reader criteria retain keyword and tag" $
        status reader == PublishedOnly
            && keyword reader == Just "syntax"
            && tags reader == [tag]
    check "empty keyword is rejected" $
        newReaderCriteria 1 Nothing (Just "") []
            == Left (createInvariantViolation "ArticleSearch" "keyword must contain 1 to 100 characters")
    check "overlong keyword is rejected" $
        newReaderCriteria 1 Nothing (Just (Text.replicate 101 "x")) []
            == Left (createInvariantViolation "ArticleSearch" "keyword must contain 1 to 100 characters")
