{-# LANGUAGE OverloadedStrings #-}

module Support.Helper.API.Request
  ( mkArticleCreatedRequestBody,
    mkArticleEditedRequestBody,
    mkArticleTerminatedRequestBody,
    mkMemoCreatedRequestBody,
    mkMemoEditedRequestBody,
    mkMemoTerminatedRequestBody,
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

mkMemoEntryJSON :: Int -> Value
mkMemoEntryJSON seed =
  object
    [ "text" .= ("entry-" <> show seed),
      "createdAt" .= seedToTime seed
    ]

mkMemoPayloadJSON :: Int -> Value
mkMemoPayloadJSON seed =
  object
    [ "identifier" .= ("memo-" <> show seed),
      "title" .= ("title-" <> show seed),
      "slug" .= ("slug-" <> show seed),
      "entries" .= [mkMemoEntryJSON seed],
      "tags" .= (["tag-" <> show seed] :: [String]),
      "status" .= ("published" :: String),
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

mkArticleEditedRequestBody :: Int -> LBS.ByteString
mkArticleEditedRequestBody seed =
  mkEventJSON seed "article.edited" $
    object
      [ "next" .= mkArticlePayloadJSON (seed + 1),
        "before" .= mkArticlePayloadJSON seed
      ]

mkArticleTerminatedRequestBody :: Int -> LBS.ByteString
mkArticleTerminatedRequestBody seed =
  mkEventJSON seed "article.terminated" (object ["article" .= ("article-" <> show seed :: String)])

mkMemoCreatedRequestBody :: Int -> LBS.ByteString
mkMemoCreatedRequestBody seed =
  mkEventJSON seed "memo.created" (object ["snapshot" .= mkMemoPayloadJSON seed])

mkMemoEditedRequestBody :: Int -> LBS.ByteString
mkMemoEditedRequestBody seed =
  mkEventJSON seed "memo.edited" $
    object
      [ "next" .= mkMemoPayloadJSON (seed + 1),
        "before" .= mkMemoPayloadJSON seed
      ]

mkMemoTerminatedRequestBody :: Int -> LBS.ByteString
mkMemoTerminatedRequestBody seed =
  mkEventJSON seed "memo.terminated" (object ["memo" .= ("memo-" <> show seed :: String)])

mkPubSubRequestBody :: LBS.ByteString -> LBS.ByteString
mkPubSubRequestBody eventJSON =
  let base64Data = BS.unpack (B64.encode (LBS.toStrict eventJSON))
   in encode $ object ["message" .= object ["data" .= base64Data]]
