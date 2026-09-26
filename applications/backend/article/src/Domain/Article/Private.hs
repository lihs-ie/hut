module Domain.Article.Private (
    PrivateArticle,
    takeDown,
    resumePublication,
) where

import Data.Time (UTCTime)
import Domain.Article.Common (ArticleIdentifier, PublicationContent, amendTimeline)
import Domain.Article.Draft (ReadyToPublish, newReadyToPublish)
import Domain.Article.Published (PublishedArticle)
import GHC.Records (HasField (..))
import Shared.Domain.Date (Timeline)
import Shared.Domain.Error (DomainError)

data PrivateArticle
    = PrivateArticle
        ArticleIdentifier
        PublicationContent
        Timeline
        UTCTime
    deriving stock (Show, Eq)

takeDown :: UTCTime -> PublishedArticle -> Either DomainError PrivateArticle
takeDown timestamp article = do
    timeline <- amendTimeline timestamp article.timeline
    pure (PrivateArticle article.identifier article.publication timeline article.publishedAt)

resumePublication :: UTCTime -> PrivateArticle -> Either DomainError ReadyToPublish
resumePublication timestamp article = do
    timeline <- amendTimeline timestamp article.timeline
    pure (newReadyToPublish article.identifier article.publication timeline)

instance HasField "identifier" PrivateArticle ArticleIdentifier where
    getField (PrivateArticle value _ _ _) = value

instance HasField "publication" PrivateArticle PublicationContent where
    getField (PrivateArticle _ value _ _) = value

instance HasField "timeline" PrivateArticle Timeline where
    getField (PrivateArticle _ _ value _) = value

instance HasField "publishedAt" PrivateArticle UTCTime where
    getField (PrivateArticle _ _ _ value) = value
