module Article.Worker.Excerpt.ContractSpec (run) where

import Article.Worker.Excerpt.Contract (claimRequest, decodeClaimedContent, interpretClaimResponse)
import Cloudflare.Workers.Headers (headerLookup, headersFromList)
import Cloudflare.Workers.HTTP (
    Method (GET),
    Request (..),
    ResponseBody (ResponseBodyBytes),
    Status (Status),
    createResponse,
    requestPath,
 )
import Cloudflare.Workers.URL (urlQueryParam)
import Control.Monad (forM_)
import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.List (nub)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Article (articleIdentifierText)
import Domain.Article.Common (ProofreadedContent, contentText, imageReferenceText, titleText)
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerationRequested (..),
    generationRequestIdentifierText,
    newGenerationRequestIdentifier,
 )
import Shared.Domain.Common.Primitive (newPositiveInteger)
import Shared.Domain.Error (DomainError (..), ServiceUnavailableError (..))
import Shared.Domain.Slug (slugText)
import Shared.Domain.Tag (tagIdentifierText)
import Shared.Infrastructure.Versioning (newVersion, versionInteger)
import TestSupport (check, identifier, right)

data Claim = Claim
    { claimedArticle :: Text
    , claimedRequest :: Text
    , claimedRevision :: Integer
    , claimedTitle :: Text
    , claimedBody :: Text
    , claimedSlug :: Text
    , claimedTags :: [Text]
    , claimedImages :: [Text]
    }

run :: IO ()
run = do
    article <- right identifier
    requestIdentifier <- right (newGenerationRequestIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    revision <- newVersion <$> right (newPositiveInteger 3)
    let request = ExcerptGenerationRequested requestIdentifier article revision
        valid = Claim
            (articleIdentifierText article)
            (generationRequestIdentifierText requestIdentifier)
            (versionInteger revision)
            "Haskell" "Body" "haskell-syntax" [] []
        mismatch = "claim response does not match the queued request"
        invalidContent = "claim response contains invalid article content"
        decode = decodeClaimedContent request

    outgoing <- right (claimRequest request)
    check "claim uses GET" (outgoing.requestMethodField == GET)
    check "claim targets the Article DO" $
        requestPath outgoing == "/internal/excerpt-generation/claim"
    check "claim sends the article identifier" $
        urlQueryParam "article" outgoing.requestURLField == Just (articleIdentifierText article)
    check "claim sends the generation request identifier" $
        urlQueryParam "request" outgoing.requestURLField
            == Just (generationRequestIdentifierText requestIdentifier)
    check "claim sends the expected revision" $
        urlQueryParam "expectedRevision" outgoing.requestURLField == Just "3"
    check "claim accepts JSON" $
        headerLookup "accept" outgoing.requestHeaders == Just "application/json"
    check "claim has no request body" $ case outgoing.requestBodyReaderField of
        Nothing -> True
        Just _ -> False

    assertAccepted "minimal valid claim" valid (decode (claimBytes valid))
    let rich = valid
            { claimedTitle = Text.replicate 100 "T"
            , claimedBody = "A body with two managed images"
            , claimedSlug = "haskell-2"
            , claimedTags = ["haskell", "workers", "haskell"]
            , claimedImages = ["01ARZ3NDEKTSV4RRFFQ69G5FAV", "01ARZ3NDEKTSV4RRFFQ69G5FAX"]
            }
    assertAccepted "valid title, slug, deduplicated tags and distinct images" rich
        (decode (claimBytes rich))

    forM_
        [ ("another article", valid{claimedArticle = "01ARZ3NDEKTSV4RRFFQ69G5FAX"})
        , ("another request", valid{claimedRequest = "01ARZ3NDEKTSV4RRFFQ69G5FAY"})
        , ("another revision", valid{claimedRevision = 4})
        ] $ \(label, claim) ->
            assertUnavailable label mismatch (decode (claimBytes claim))

    forM_
        [ ("blank title", valid{claimedTitle = " \t "})
        , ("overlong title", valid{claimedTitle = Text.replicate 101 "T"})
        , ("blank body", valid{claimedBody = " \n "})
        , ("empty slug", valid{claimedSlug = ""})
        , ("uppercase slug", valid{claimedSlug = "Haskell"})
        , ("double-hyphen slug", valid{claimedSlug = "haskell--syntax"})
        , ("empty tag", valid{claimedTags = [""]})
        , ("overlong tag", valid{claimedTags = [Text.replicate 256 "t"]})
        , ("invalid image ULID", valid{claimedImages = ["not-a-ulid"]})
        , ("duplicate images", valid
            { claimedImages = ["01ARZ3NDEKTSV4RRFFQ69G5FAV", "01ARZ3NDEKTSV4RRFFQ69G5FAV"] })
        ] $ \(label, claim) ->
            assertUnavailable label invalidContent (decode (claimBytes claim))

    forM_ ["{", "null", "[]", "{}"] $ \bytes ->
        assertMalformed ("malformed JSON " <> show bytes) (decode bytes)
    forM_ ["article", "request", "expectedRevision", "content"] $ \key ->
        assertMalformed ("missing root field " <> show key) $
            decode (jsonBytes (removeKey key (claimValue valid)))
    forM_ ["title", "body", "slug", "tags", "images"] $ \key ->
        assertMalformed ("missing content field " <> show key) $
            decode (jsonBytes (modifyContent (removeKey key) (claimValue valid)))
    forM_
        [ ("article is numeric", replaceKey "article" (toJSON (3 :: Int)))
        , ("request is boolean", replaceKey "request" (Bool True))
        , ("revision is text", replaceKey "expectedRevision" (String "3"))
        , ("content is text", replaceKey "content" (String "Body"))
        , ("title is numeric", modifyContent (replaceKey "title" (toJSON (3 :: Int))))
        , ("body is boolean", modifyContent (replaceKey "body" (Bool False)))
        , ("slug is null", modifyContent (replaceKey "slug" Null))
        , ("tags is text", modifyContent (replaceKey "tags" (String "haskell")))
        , ("images contains a number", modifyContent (replaceKey "images" (toJSON [3 :: Int])))
        ] $ \(label, change) ->
            assertMalformed label (decode (jsonBytes (change (claimValue valid))))

    let http status bytes = createResponse (Status status) (headersFromList []) (ResponseBodyBytes bytes)
    check "HTTP 204 ignores the body and ends the claim" $
        interpretClaimResponse request (http 204 "not JSON") == Right Nothing
    case interpretClaimResponse request (http 200 (claimBytes rich)) of
        Right (Just content) -> checkContent "HTTP 200" rich content
        other -> fail ("HTTP 200 should return claimed content: " <> show other)
    case interpretClaimResponse request (http 200 "{}") of
        Left err -> assertMalformed "HTTP 200 malformed body" (Left err)
        Right _ -> fail "HTTP 200 malformed body must retry"
    forM_ [201, 202, 400, 409, 500, 503] $ \status ->
        case interpretClaimResponse request (http status "") of
            Left err -> check ("HTTP " <> show status <> " retries with its status") $
                err == ServiceUnavailable (ServiceUnavailableError "ArticleDO"
                    ("claim returned HTTP " <> Text.pack (show status)))
            Right _ -> fail ("HTTP " <> show status <> " must retry")

claimValue :: Claim -> Value
claimValue claim = object
    [ "article" .= claim.claimedArticle
    , "request" .= claim.claimedRequest
    , "expectedRevision" .= claim.claimedRevision
    , "content" .= object
        [ "title" .= claim.claimedTitle
        , "body" .= claim.claimedBody
        , "slug" .= claim.claimedSlug
        , "tags" .= claim.claimedTags
        , "images" .= claim.claimedImages
        ]
    ]

claimBytes :: Claim -> ByteString
claimBytes = jsonBytes . claimValue

jsonBytes :: Value -> ByteString
jsonBytes = Lazy.toStrict . encode

removeKey :: Key -> Value -> Value
removeKey key (Object fields) = Object (KeyMap.delete key fields)
removeKey _ value = value

replaceKey :: Key -> Value -> Value -> Value
replaceKey key replacement (Object fields) = Object (KeyMap.insert key replacement fields)
replaceKey _ _ value = value

modifyContent :: (Value -> Value) -> Value -> Value
modifyContent change (Object fields) =
    case KeyMap.lookup "content" fields of
        Just content -> Object (KeyMap.insert "content" (change content) fields)
        Nothing -> Object fields
modifyContent _ value = value

assertAccepted :: String -> Claim -> Either DomainError ProofreadedContent -> IO ()
assertAccepted label claim result = do
    content <- either (fail . ((label <> ": ") <>) . show) pure result
    checkContent label claim content

checkContent :: String -> Claim -> ProofreadedContent -> IO ()
checkContent label claim content = do
    check (label <> " title") (titleText content.title == claim.claimedTitle)
    check (label <> " body") (contentText content.body == claim.claimedBody)
    check (label <> " slug") (slugText content.slug == claim.claimedSlug)
    check (label <> " tags") (map tagIdentifierText content.tags == nub claim.claimedTags)
    check (label <> " images") $
        Set.map imageReferenceText content.images == Set.fromList claim.claimedImages

assertUnavailable :: String -> Text -> Either DomainError a -> IO ()
assertUnavailable label reason result =
    case result of
        Left err -> check label $
            err == ServiceUnavailable (ServiceUnavailableError "ArticleDO" reason)
        Right _ -> fail (label <> " must be rejected")

assertMalformed :: String -> Either DomainError a -> IO ()
assertMalformed label result =
    case result of
        Left (ServiceUnavailable (ServiceUnavailableError name reason)) ->
            check (label <> ": " <> Text.unpack reason) $
                name == "ArticleDO" && not (Text.null reason)
        _ -> fail (label <> " must be rejected as malformed")
