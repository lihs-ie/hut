module Media.Presentation.API.RetryImageInspection (
    RetryImageInspectionResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype RetryImageInspectionResponse = RetryImageInspectionResponse
    { uploadAttemptIdentifier :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
