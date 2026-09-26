module Domain.Article.Published (
    PublishedArticle,
    publish,
) where

import Data.Time (UTCTime)
import Domain.Article.Common (ArticleIdentifier, PublicationContent, amendTimeline)
import Domain.Article.Draft (
    ReadyToPublish,
    draftIdentifier,
    draftTimeline,
    publicationContent,
 )
import GHC.Records (HasField (..))
import Shared.Domain.Date (Timeline)
import Shared.Domain.Error (DomainError)

data PublishedArticle
    = PublishedArticle
        ArticleIdentifier
        PublicationContent
        Timeline
        UTCTime
    deriving stock (Show, Eq)

publish :: UTCTime -> ReadyToPublish -> Either DomainError PublishedArticle
publish timestamp draft = do
    timeline <- amendTimeline timestamp (draftTimeline draft)
    pure (PublishedArticle (draftIdentifier draft) (publicationContent draft) timeline timestamp)

instance HasField "identifier" PublishedArticle ArticleIdentifier where
    getField (PublishedArticle value _ _ _) = value

instance HasField "publication" PublishedArticle PublicationContent where
    getField (PublishedArticle _ value _ _) = value

instance HasField "timeline" PublishedArticle Timeline where
    getField (PublishedArticle _ _ value _) = value

instance HasField "publishedAt" PublishedArticle UTCTime where
    getField (PublishedArticle _ _ _ value) = value
