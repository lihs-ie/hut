module Media.Presentation.API.GetImageStatus (
    GetImageStatusResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data GetImageStatusResponse = GetImageStatusResponse
    { imageIdentifier :: Text
    , state :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
