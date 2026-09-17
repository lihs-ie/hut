module Media.Presentation.API.RetryImageUpload (
    RetryImageUploadResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data RetryImageUploadResponse = RetryImageUploadResponse
    { imageIdentifier :: Text
    , uploadAttemptIdentifier :: Text
    , uploadDestination :: Text
    , expiresAt :: UTCTime
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
