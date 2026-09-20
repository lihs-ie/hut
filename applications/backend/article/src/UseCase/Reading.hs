module UseCase.Reading (
    module Domain.Article.Criteria,
    matchesFilter,
    pageResult,
) where

import Domain.Article (Article (..))
import Domain.Article.Criteria
import Shared.Domain.Error (DomainError, createUnexpectedError)
import Shared.Domain.Pager (Pager)
import Shared.Domain.Pager qualified as Pager

matchesFilter :: ArticleFilter -> Article -> Bool
matchesFilter AllArticles _ = True
matchesFilter UnvalidatedOnly (Unvalidated _) = True
matchesFilter ProofreadedOnly (Proofreaded _) = True
matchesFilter ReadyOnly (Ready _) = True
matchesFilter PublishedOnly (Published _) = True
matchesFilter PrivateOnly (Private _) = True
matchesFilter _ _ = False

pageResult :: Criteria -> Int -> [article] -> Either DomainError Pager
pageResult criteria total articles = do
    pager <- Pager.newPager total (pageSize criteria) (pageNumber criteria)
    let expected = min (pageSize criteria) (max 0 (total - pageOffset criteria))
    if length articles /= expected
        then Left (createUnexpectedError "Article" "page count and total do not agree")
        else Right pager
