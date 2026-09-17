module Media.Presentation.API.RequestImageUpload (
    RequestImageUploadRequest (..),
    RequestImageUploadResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data RequestImageUploadRequest = RequestImageUploadRequest
    { contentType :: Text
    , byteSize :: Integer
    , sha256 :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data RequestImageUploadResponse = RequestImageUploadResponse
    { imageIdentifier :: Text
    , uploadAttemptIdentifier :: Text
    , uploadDestination :: Text
    , expiresAt :: UTCTime
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
