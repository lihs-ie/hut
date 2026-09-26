module Domain.Article.Criteria (
    Criteria,
    ArticleFilter (..),
    newCriteria,
    status,
    pageNumber,
    pageSize,
    pageOffset,
) where

import Data.Maybe (fromMaybe)
import Shared.Domain.Error (DomainError, createInvariantViolation)
import Shared.Domain.Pager qualified as Pager

-- Search selection, not a second representation of aggregate state.
data ArticleFilter = AllArticles | UnvalidatedOnly | ProofreadedOnly | ReadyOnly | PublishedOnly | PrivateOnly
    deriving stock (Show, Eq)

data Criteria = Criteria ArticleFilter Int Int Int
    deriving stock (Show, Eq)

status :: Criteria -> ArticleFilter
status (Criteria value _ _ _) = value

pageNumber :: Criteria -> Int
pageNumber (Criteria _ value _ _) = value

pageSize :: Criteria -> Int
pageSize (Criteria _ _ value _) = value

pageOffset :: Criteria -> Int
pageOffset (Criteria _ _ _ value) = value

newCriteria :: ArticleFilter -> Int -> Maybe Int -> Either DomainError Criteria
newCriteria selection current requested = do
    let items = fromMaybe 10 requested
    if items > 100
        then Left (createInvariantViolation "Pagination" "at most 100 articles per page")
        else do
            pager <- Pager.newPager 0 items current
            pure (Criteria selection current items (Pager.offset pager))
