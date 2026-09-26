{-# LANGUAGE DeriveAnyClass #-}

module Presentation.API (
    ArticleAdminAPI,
    ArticleAdminRoutes (..),
    ArticleReaderRoutes (..),
    ArticleAPI,
    CorrelatedResponse,
    ProofreadResponse (..),
    RegenerationResponse (..),
    SlugAvailabilityResponse (..),
    DraftRequest (..),
    ExcerptRequest (..),
    DiscardResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Presentation.API.ArticleView (ArticlePage, ArticleView, ReaderArticlePage)
import Servant.API

type CorrelatedResponse body =
    Headers '[Header "X-Correlation-Identifier" Text] body

data ProofreadResponse = ProofreadResponse
    { article :: Text
    , phase :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data RegenerationResponse = RegenerationResponse
    { article :: Text
    , requestIdentifier :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data SlugAvailabilityResponse = SlugAvailabilityResponse
    { available :: Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data DraftRequest = DraftRequest
    { title :: Text
    , body :: Text
    , slug :: Maybe Text
    , tags :: [Text]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype ExcerptRequest = ExcerptRequest {excerpt :: Text}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype DiscardResponse = DiscardResponse {article :: Text}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ArticleAdminRoutes mode = ArticleAdminRoutes
    { jotDown ::
        mode
            :- "articles"
                :> ReqBody '[JSON] DraftRequest
                :> PostCreated '[JSON] (CorrelatedResponse ArticleView)
    , browseArticles ::
        mode
            :- "articles"
                :> QueryParam "page" Text
                :> QueryParam "size" Text
                :> QueryParam "status" Text
                :> Get '[JSON] (CorrelatedResponse ArticlePage)
    , viewArticle ::
        mode
            :- "articles"
                :> Capture "articleIdentifier" Text
                :> Get '[JSON] (CorrelatedResponse ArticleView)
    , amendDraft ::
        mode
            :- "articles"
                :> Capture "articleIdentifier" Text
                :> "draft"
                :> ReqBody '[JSON] DraftRequest
                :> Put '[JSON] (CorrelatedResponse ArticleView)
    , reviseExcerpt ::
        mode
            :- "articles"
                :> Capture "articleIdentifier" Text
                :> "excerpt"
                :> ReqBody '[JSON] ExcerptRequest
                :> Patch '[JSON] (CorrelatedResponse ArticleView)
    , publish ::
        mode
            :- "articles"
                :> Capture "articleIdentifier" Text
                :> "publication"
                :> Post '[JSON] (CorrelatedResponse ArticleView)
    , takeDown ::
        mode
            :- "articles"
                :> Capture "articleIdentifier" Text
                :> "publication"
                :> Delete '[JSON] (CorrelatedResponse ArticleView)
    , resumePublication ::
        mode
            :- "articles"
                :> Capture "articleIdentifier" Text
                :> "publication-resumptions"
                :> Post '[JSON] (CorrelatedResponse ArticleView)
    , discardArticle ::
        mode
            :- "articles"
                :> Capture "articleIdentifier" Text
                :> Delete '[JSON] (CorrelatedResponse DiscardResponse)
    , checkSlugAvailability ::
        mode
            :- "articles"
                :> Capture "articleIdentifier" Text
                :> "slug-availability"
                :> QueryParam "slug" Text
                :> Get '[JSON] (CorrelatedResponse SlugAvailabilityResponse)
    , proofread ::
        mode
            :- "articles"
                :> Capture "articleIdentifier" Text
                :> "proofreading"
                :> Post '[JSON] (CorrelatedResponse ProofreadResponse)
    , requestExcerptRegeneration ::
        mode
            :- "articles"
                :> Capture "articleIdentifier" Text
                :> "excerpt-generation-requests"
                :> Post '[JSON] (CorrelatedResponse RegenerationResponse)
    }
    deriving stock (Generic)

type ArticleAdminAPI =
    "admin"
        :> Header "X-Hut-Actor" Text
        :> Header "X-Correlation-Identifier" Text
        :> NamedRoutes ArticleAdminRoutes

data ArticleReaderRoutes mode = ArticleReaderRoutes
    { browseArticles ::
        mode
            :- QueryParam "page" Text
                :> QueryParam "size" Text
                :> QueryParam "q" Text
                :> QueryParams "tag" Text
                :> Get '[JSON] (CorrelatedResponse ReaderArticlePage)
    , readArticle ::
        mode
            :- Capture "slug" Text
                :> Get '[JSON] (CorrelatedResponse ArticleView)
    }
    deriving stock (Generic)

type ArticleReaderAPI =
    "articles"
        :> Header "X-Correlation-Identifier" Text
        :> NamedRoutes ArticleReaderRoutes

type ArticleAPI = ArticleAdminAPI :<|> ArticleReaderAPI
