{-# LANGUAGE TypeFamilies #-}

module UseCase.Result (ArticleUseCase (..), ArticleEventsFor) where

import Data.Kind (Type)
import Domain.Article.Event (ArticleDiscarded, ArticleDraftAmended, ArticleDraftStarted, ArticleProofreaded, ArticlePublished, ArticleReadyToPublish, ArticleTakenDown)

data ArticleUseCase
    = JotDown
    | AmendDraft
    | Proofread
    | PrepareToPublish
    | Publish
    | TakeDown
    | ResumePublication
    | DiscardArticle
    | BrowseArticlesForAdmin
    | ViewArticleForAdmin
    | BrowseArticlesForReader
    | ReadArticle
    | CheckSlugAvailability
    | RequestExcerptRegeneration

type family ArticleEventsFor (useCase :: ArticleUseCase) :: [Type] where
    ArticleEventsFor 'JotDown = '[ArticleDraftStarted]
    ArticleEventsFor 'AmendDraft = '[ArticleDraftAmended]
    ArticleEventsFor 'Proofread = '[ArticleProofreaded]
    ArticleEventsFor 'PrepareToPublish = '[ArticleReadyToPublish]
    ArticleEventsFor 'Publish = '[ArticlePublished]
    ArticleEventsFor 'TakeDown = '[ArticleTakenDown]
    ArticleEventsFor 'ResumePublication = '[]
    ArticleEventsFor 'DiscardArticle = '[ArticleDiscarded]
    ArticleEventsFor 'BrowseArticlesForAdmin = '[]
    ArticleEventsFor 'ViewArticleForAdmin = '[]
    ArticleEventsFor 'BrowseArticlesForReader = '[]
    ArticleEventsFor 'ReadArticle = '[]
    ArticleEventsFor 'CheckSlugAvailability = '[]
    ArticleEventsFor 'RequestExcerptRegeneration = '[]
