module UseCase.Helper (requireArticle) where

import Domain.Article (Article, ArticleIdentifier, FindArticle, articleIdentifier)
import Domain.Article.Common (articleIdentifierText)
import Shared.Domain.Common.Transaction (Transaction, fromEither)
import Shared.Domain.Error (createAggregateNotFound, createUnexpectedError)

requireArticle ::
    (Monad m) =>
    FindArticle (Transaction context m) ->
    ArticleIdentifier ->
    Transaction context m Article
requireArticle find requested = do
    found <- find requested
    fromEither $ case found of
        Nothing -> Left (createAggregateNotFound "Article" (articleIdentifierText requested))
        Just article
            | articleIdentifier article == requested -> Right article
            | otherwise -> Left (createUnexpectedError "Article" "loaded identity does not match request")
