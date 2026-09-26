module Media.Presentation.API.FindAvailableImages (
    FindAvailableImagesRequest (..),
    FindAvailableImagesResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype FindAvailableImagesRequest = FindAvailableImagesRequest
    { images :: [Text]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype FindAvailableImagesResponse = FindAvailableImagesResponse
    { available :: [Text]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
