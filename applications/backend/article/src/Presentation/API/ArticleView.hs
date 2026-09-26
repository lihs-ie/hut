{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PackageImports #-}

module Presentation.API.ArticleView (
    ArticleView (..),
    ArticlePage (..),
    PageView (..),
    articleView,
    publishedView,
    pageView,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import "article" Domain.Article (
    Article (..),
    ArticleIdentifier,
    ImageReference,
    articleIdentifierText,
    imageReferenceText,
 )
import "article" Domain.Article.Common (contentText, draftBodyText, titleText)
import "article" Domain.Article.Draft (
    draftContent,
    draftIdentifier,
    draftTimeline,
    proofreadedContent,
    publicationContent,
 )
import "article" Domain.Article.Published (PublishedArticle)
import "shared" Shared.Domain.Date (Timeline)
import "shared" Shared.Domain.Excerpt (excerptText)
import "shared" Shared.Domain.Pager qualified as Pager
import "shared" Shared.Domain.Slug (slugText)
import "shared" Shared.Domain.Tag (TagIdentifier, tagIdentifierText)

data ArticleView = ArticleView
    { identifier :: Text
    , phase :: Text
    , title :: Text
    , body :: Text
    , slug :: Maybe Text
    , excerpt :: Maybe Text
    , tags :: [Text]
    , images :: [Text]
    , createdAt :: UTCTime
    , updatedAt :: UTCTime
    , publishedAt :: Maybe UTCTime
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data PageView = PageView
    { total :: Int
    , items :: Int
    , current :: Int
    , firstPage :: Int
    , lastPage :: Int
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ArticlePage = ArticlePage
    { articles :: [ArticleView]
    , pagination :: PageView
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

articleView :: Article -> ArticleView
articleView (Unvalidated draft) =
    let content = draftContent draft
     in viewBase (draftIdentifier draft) "unvalidated" (draftTimeline draft)
            (titleText content.title) (draftBodyText content.body)
            (slugText <$> content.slug) Nothing content.tags content.images Nothing
articleView (Proofreaded draft) =
    let content = proofreadedContent draft
     in viewBase (draftIdentifier draft) "proofreaded" (draftTimeline draft)
            (titleText content.title) (contentText content.body)
            (Just (slugText content.slug)) Nothing content.tags content.images Nothing
articleView (Ready draft) =
    let content = publicationContent draft
     in viewBase (draftIdentifier draft) "ready" (draftTimeline draft)
            (titleText content.title) (contentText content.body)
            (Just (slugText content.slug)) (Just (excerptText content.excerpt))
            content.tags content.images Nothing
articleView (Published article) = publishedView article
articleView (Private article) =
    let content = article.publication
     in viewBase article.identifier "private" article.timeline
            (titleText content.title) (contentText content.body)
            (Just (slugText content.slug)) (Just (excerptText content.excerpt))
            content.tags content.images (Just article.publishedAt)

publishedView :: PublishedArticle -> ArticleView
publishedView article =
    let content = article.publication
     in viewBase article.identifier "published" article.timeline
            (titleText content.title) (contentText content.body)
            (Just (slugText content.slug)) (Just (excerptText content.excerpt))
            content.tags content.images (Just article.publishedAt)

pageView :: Pager.Pager -> PageView
pageView pager =
    PageView
        { total = Pager.total pager
        , items = Pager.items pager
        , current = Pager.current pager
        , firstPage = Pager.firstPage pager
        , lastPage = Pager.lastPage pager
        }

viewBase ::
    ArticleIdentifier -> Text -> Timeline -> Text -> Text -> Maybe Text -> Maybe Text ->
    [TagIdentifier] -> Set.Set ImageReference -> Maybe UTCTime -> ArticleView
viewBase article phase timeline title body slug excerpt tags images publishedAt =
    ArticleView
        { identifier = articleIdentifierText article
        , phase
        , title
        , body
        , slug
        , excerpt
        , tags = map tagIdentifierText tags
        , images = map imageReferenceText (Set.toAscList images)
        , createdAt = timeline.createdAt
        , updatedAt = timeline.updatedAt
        , publishedAt
        }
