module Domain.Article.Criteria (
    Criteria,
    ArticleFilter (..),
    newCriteria,
    newReaderCriteria,
    status,
    keyword,
    tags,
    pageNumber,
    pageSize,
    pageOffset,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Shared.Domain.Error (DomainError, createInvariantViolation)
import Shared.Domain.Pager qualified as Pager
import Shared.Domain.Tag (TagIdentifier)

-- Search selection, not a second representation of aggregate state.
data ArticleFilter = AllArticles | UnvalidatedOnly | ProofreadedOnly | ReadyOnly | PublishedOnly | PrivateOnly
    deriving stock (Show, Eq)

data Criteria = Criteria ArticleFilter Int Int Int (Maybe Text) [TagIdentifier]
    deriving stock (Show, Eq)

status :: Criteria -> ArticleFilter
status (Criteria value _ _ _ _ _) = value

keyword :: Criteria -> Maybe Text
keyword (Criteria _ _ _ _ value _) = value

tags :: Criteria -> [TagIdentifier]
tags (Criteria _ _ _ _ _ value) = value

pageNumber :: Criteria -> Int
pageNumber (Criteria _ value _ _ _ _) = value

pageSize :: Criteria -> Int
pageSize (Criteria _ _ value _ _ _) = value

pageOffset :: Criteria -> Int
pageOffset (Criteria _ _ _ value _ _) = value

newCriteria :: ArticleFilter -> Int -> Maybe Int -> Either DomainError Criteria
newCriteria selection current requested = do
    let items = fromMaybe 10 requested
    if items > 100
        then Left (createInvariantViolation "Pagination" "at most 100 articles per page")
        else do
            pager <- Pager.newPager 0 items current
            pure (Criteria selection current items (Pager.offset pager) Nothing [])

newReaderCriteria ::
    Int -> Maybe Int -> Maybe Text -> [TagIdentifier] -> Either DomainError Criteria
newReaderCriteria current requested searchTerm selectedTags = do
    base <- newCriteria PublishedOnly current requested
    case searchTerm of
        Just value | Text.null value || Text.length value > 100 ->
            Left (createInvariantViolation "ArticleSearch" "keyword must contain 1 to 100 characters")
        _ -> pure $ Criteria
            PublishedOnly
            (pageNumber base)
            (pageSize base)
            (pageOffset base)
            searchTerm
            selectedTags
