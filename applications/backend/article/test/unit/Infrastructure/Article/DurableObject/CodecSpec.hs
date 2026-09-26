module Infrastructure.Article.DurableObject.CodecSpec (run) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), decode, encode, toJSON)
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as Lazy
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Domain.Article (Article (..))
import Domain.Article.Draft (prepareToPublish, proofread)
import Domain.Article.Private (takeDown)
import Domain.Article.Published (publish)
import Infrastructure.Article.DurableObject.Codec (articleCodec)
import Infrastructure.Article.DurableObject.Repository (ArticleCodec (..))
import Shared.Domain.Excerpt (newExcerpt)
import TestSupport (check, confirmed, right, start, timestamp)

run :: IO ()
run = do
    draft <- right start
    available <- right confirmed
    proof <- right (proofread (timestamp 1) available draft)
    excerpt <- right (newExcerpt "A concise summary")
    ready <- right (prepareToPublish (timestamp 2) excerpt proof)
    published <- right (publish (timestamp 3) ready)
    private <- right (takeDown (timestamp 4) published)
    let states =
            [ Unvalidated draft
            , Proofreaded proof
            , Ready ready
            , Published published
            , Private private
            ]
    mapM_ checkRoundTrip states
    encoded <- right (articleCodec.encodeArticle (Published published))
    check "unknown phase rejected" $ isLeft $
        articleCodec.decodeArticle (Text.replace "published" "unknown" encoded)
    check "publishedAt cannot precede creation" $ isLeft $
        articleCodec.decodeArticle
            (Text.replace "2026-01-01T00:00:03Z" "2025-01-01T00:00:00Z" encoded)
    check "blank body rejected" $ isLeft $
        articleCodec.decodeArticle
            (Text.replace "Body with managed-image" "" encoded)
    rejectsCorruptStorage states

rejectsCorruptStorage :: [Article] -> IO ()
rejectsCorruptStorage states = do
    let [unvalidated, proofreaded, ready, published, private] = states
        invalidTime = String "2025-01-01T00:00:00Z"
        image = String "01ARZ3NDEKTSV4RRFFQ69G5FAV"
        corruptions =
            [ ("malformed JSON", unvalidated, const "not-json")
            , ("missing phase", unvalidated, change (KeyMap.delete "phase"))
            , ("missing identifier", unvalidated, change (KeyMap.delete "identifier"))
            , ("invalid identifier", unvalidated, set "identifier" (String "invalid"))
            , ("duplicate images", unvalidated, set "images" (toJSON [image, image]))
            , ("invalid image", unvalidated, set "images" (toJSON [String "invalid"]))
            , ("updated before created", unvalidated, set "updatedAt" invalidTime)
            , ("unvalidated excerpt", unvalidated, set "excerpt" (String "summary"))
            , ("unvalidated publishedAt", unvalidated, set "publishedAt" (String "2026-01-01T00:00:00Z"))
            , ("proofreaded excerpt", proofreaded, set "excerpt" (String "summary"))
            , ("proofreaded publishedAt", proofreaded, set "publishedAt" (String "2026-01-01T00:00:00Z"))
            , ("ready missing excerpt", ready, set "excerpt" Null)
            , ("ready publishedAt", ready, set "publishedAt" (String "2026-01-01T00:00:00Z"))
            , ("published missing excerpt", published, set "excerpt" Null)
            , ("published missing date", published, set "publishedAt" Null)
            , ("private missing excerpt", private, set "excerpt" Null)
            , ("private missing date", private, set "publishedAt" Null)
            ]
    forM_ corruptions $ \(label, original, alter) -> do
        encoded <- right (articleCodec.encodeArticle original)
        check label (isLeft (articleCodec.decodeArticle (alter encoded)))

set :: Key -> Value -> Text -> Text
set key value = change (KeyMap.insert key value)

change :: (KeyMap.KeyMap Value -> KeyMap.KeyMap Value) -> Text -> Text
change update raw =
    case decode (Lazy.fromStrict (encodeUtf8 raw)) of
        Just (Object fields) -> decodeUtf8 (Lazy.toStrict (encode (Object (update fields))))
        _ -> error "test fixture must contain an object"

checkRoundTrip :: Article -> IO ()
checkRoundTrip article = do
    encoded <- right (articleCodec.encodeArticle article)
    decoded <- right (articleCodec.decodeArticle encoded)
    check "all article phases round-trip through the storage codec" (decoded == article)
