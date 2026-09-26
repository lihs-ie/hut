{-# LANGUAGE DeriveAnyClass #-}

module Presentation.API (
    ArticleAdminAPI,
    ArticleAdminRoutes (..),
    CorrelatedResponse,
    ProofreadResponse (..),
    RegenerationResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
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

data ArticleAdminRoutes mode = ArticleAdminRoutes
    { proofread ::
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
