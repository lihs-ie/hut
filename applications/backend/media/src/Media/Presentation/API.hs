module Media.Presentation.API (
    MediaAPI,
    MediaRoutes (..),
    CorrelatedResponse,
    ErrorResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Media.Presentation.API.FindAvailableImages (
    FindAvailableImagesRequest,
    FindAvailableImagesResponse,
 )
import Media.Presentation.API.GetImageStatus (GetImageStatusResponse)
import Media.Presentation.API.RequestImageUpload (
    RequestImageUploadRequest,
    RequestImageUploadResponse,
 )
import Media.Presentation.API.RetryImageInspection (RetryImageInspectionResponse)
import Media.Presentation.API.RetryImageUpload (RetryImageUploadResponse)
import Servant.API

type CorrelatedResponse body =
    Headers '[Header "X-Correlation-Identifier" Text] body

data ErrorResponse = ErrorResponse
    { code :: Text
    , message :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data MediaRoutes mode = MediaRoutes
    { requestImageUpload ::
        mode
            :- "images"
                :> ReqBody '[JSON] RequestImageUploadRequest
                :> PostCreated '[JSON] (CorrelatedResponse RequestImageUploadResponse)
    , retryImageUpload ::
        mode
            :- "images"
                :> Capture "imageIdentifier" Text
                :> "upload-attempts"
                :> Post '[JSON] (CorrelatedResponse RetryImageUploadResponse)
    , getImageStatus ::
        mode
            :- "images"
                :> Capture "imageIdentifier" Text
                :> Get '[JSON] (CorrelatedResponse GetImageStatusResponse)
    , findAvailableImages ::
        mode
            :- "images"
                :> "availability"
                :> ReqBody '[JSON] FindAvailableImagesRequest
                :> Post '[JSON] (CorrelatedResponse FindAvailableImagesResponse)
    , retryImageInspection ::
        mode
            :- "images"
                :> Capture "imageIdentifier" Text
                :> "inspection-retries"
                :> Post '[JSON] (CorrelatedResponse RetryImageInspectionResponse)
    }
    deriving stock (Generic)

type MediaAPI =
    Header "X-Hut-Actor" Text
        :> Header "X-Correlation-Identifier" Text
        :> NamedRoutes MediaRoutes
