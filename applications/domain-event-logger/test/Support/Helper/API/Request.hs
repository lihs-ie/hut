{-# LANGUAGE OverloadedStrings #-}

module Support.Helper.API.Request
  ( mkArticleCreatedRequestBody,
    mkArticleTerminatedRequestBody,
    mkPubSubRequestBody,
  )
where

import Data.Aeson (Value, encode, object, toJSON, (.=))
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

seedToTime :: Int -> Value
seedToTime seed = toJSON (posixSecondsToUTCTime (fromIntegral seed))

mkTimelineJSON :: Int -> Value
mkTimelineJSON seed =
  object
    [ "createdAt" .= seedToTime seed,
      "updatedAt" .= seedToTime (seed + 1)
    ]

mkArticlePayloadJSON :: Int -> Value
mkArticlePayloadJSON seed =
  object
    [ "identifier" .= ("article-" <> show seed),
      "title" .= ("title-" <> show seed),
      "content" .= ("content-" <> show seed),
      "excerpt" .= ("excerpt-" <> show seed),
      "slug" .= ("slug-" <> show seed),
      "status" .= ("published" :: String),
      "tags" .= (["tag-" <> show seed] :: [String]),
      "timeline" .= mkTimelineJSON seed
    ]

mkEventJSON :: Int -> String -> Value -> LBS.ByteString
mkEventJSON seed eventType payload =
  encode $
    object
      [ "identifier" .= ("evt-" <> show seed),
        "occurredAt" .= seedToTime seed,
        "type" .= eventType,
        "payload" .= payload
      ]

mkArticleCreatedRequestBody :: Int -> LBS.ByteString
mkArticleCreatedRequestBody seed =
  mkEventJSON seed "article.created" (object ["snapshot" .= mkArticlePayloadJSON seed])

mkArticleTerminatedRequestBody :: Int -> LBS.ByteString
mkArticleTerminatedRequestBody seed =
  mkEventJSON seed "article.terminated" (object ["article" .= ("article-" <> show seed :: String)])

mkPubSubRequestBody :: LBS.ByteString -> LBS.ByteString
mkPubSubRequestBody eventJSON =
  let base64Data = BS.unpack (B64.encode (LBS.toStrict eventJSON))
   in encode $ object ["message" .= object ["data" .= base64Data]]
