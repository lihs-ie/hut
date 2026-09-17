module Media.Presentation.API.RequestImageUploadSpec (run) where

import Data.Aeson (Value, decode, encode, object, (.=))
import Data.Text (Text)
import Media.Presentation.API.RequestImageUpload (
    RequestImageUploadRequest (
        RequestImageUploadRequest,
        byteSize,
        contentType,
        sha256
    ),
 )

run :: IO Bool
run = pure requestJSONExcludesMetadata

requestJSONExcludesMetadata :: Bool
requestJSONExcludesMetadata =
    decode (encode request) == Just expected
  where
    request =
        RequestImageUploadRequest
            { contentType = "image/png"
            , byteSize = 128
            , sha256 = validDigest
            }
    expected :: Value
    expected =
        object
            [ "contentType" .= ("image/png" :: Text)
            , "byteSize" .= (128 :: Integer)
            , "sha256" .= validDigest
            ]

validDigest :: Text
validDigest = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
