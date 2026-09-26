module Media.Presentation.APISpec (run) where

import Data.Aeson (Value, decode, encode, object, (.=))
import Data.Text (Text)
import Media.Presentation.API (ErrorResponse (ErrorResponse))

run :: IO Bool
run = pure responseErrorContract

responseErrorContract :: Bool
responseErrorContract =
    decode (encode response) == Just expected
  where
    response = ErrorResponse "image_not_found" "The image was not found."
    expected :: Value
    expected =
        object
            [ "code" .= ("image_not_found" :: Text)
            , "message" .= ("The image was not found." :: Text)
            ]
