module UseCase.Reading (
    ArticleFilter (..),
    matchesFilter,
    PageRequest,
    pageNumber,
    pageSize,
    pageOffset,
    newPageRequest,
    pageResult,
    ReadPage,
) where

import Data.Maybe (fromMaybe)
import Domain.Article (Article (..))
import Shared.Domain.Error (DomainError, createInvariantViolation, createUnexpectedError)
import Shared.Domain.Pager (Pager)
import Shared.Domain.Pager qualified as Pager

-- Query criteria, not a second state representation in the aggregate.
data ArticleFilter = AllArticles | UnvalidatedOnly | ProofreadedOnly | ReadyOnly | PublishedOnly | PrivateOnly
    deriving stock (Show, Eq)

matchesFilter :: ArticleFilter -> Article -> Bool
matchesFilter AllArticles _ = True
matchesFilter UnvalidatedOnly (Unvalidated _) = True
matchesFilter ProofreadedOnly (Proofreaded _) = True
matchesFilter ReadyOnly (Ready _) = True
matchesFilter PublishedOnly (Published _) = True
matchesFilter PrivateOnly (Private _) = True
matchesFilter _ _ = False

data PageRequest = PageRequest Int Int Int
    deriving stock (Show, Eq)

pageNumber :: PageRequest -> Int
pageNumber (PageRequest value _ _) = value

pageSize :: PageRequest -> Int
pageSize (PageRequest _ value _) = value

pageOffset :: PageRequest -> Int
pageOffset (PageRequest _ _ value) = value

newPageRequest :: Int -> Maybe Int -> Either DomainError PageRequest
newPageRequest current requested = do
    let items = fromMaybe 10 requested
    if items > 100
        then Left (createInvariantViolation "Pagination" "at most 100 articles per page")
        else do
            pager <- Pager.newPager 0 items current
            pure (PageRequest current items (Pager.offset pager))

-- The adapter counts and selects from one consistent snapshot with the same
-- visibility/filter. It orders BEFORE offset/limit, including identifier DESC.
type ReadPage m article = PageRequest -> m (Either DomainError (Int, [article]))

pageResult :: PageRequest -> Int -> [article] -> Either DomainError Pager
pageResult request total articles = do
    pager <- Pager.newPager total (pageSize request) (pageNumber request)
    let expected = min (pageSize request) (max 0 (total - pageOffset request))
    if length articles /= expected
        then Left (createUnexpectedError "Article" "page count and total do not agree")
        else Right pager
