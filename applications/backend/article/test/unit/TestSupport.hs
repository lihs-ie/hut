module TestSupport (
    check,
    right,
    identifier,
    input,
    timestamp,
    start,
    extractImages,
    confirmed,
    image,
    otherImage,
    emptyContent,
) where

import Control.Monad (unless)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (UTCTime, addUTCTime)
import Domain.Article.Common
import Domain.Article.Draft
import Shared.Domain.Error (DomainError)

check :: String -> Bool -> IO ()
check name passed = unless passed (fail name)

right :: (Show e) => Either e a -> IO a
right = either (fail . show) pure

identifier :: Either DomainError ArticleIdentifier
identifier = newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV"

image :: Either DomainError ImageReference
image = newImageReference "01ARZ3NDEKTSV4RRFFQ69G5FAV"

otherImage :: Either DomainError ImageReference
otherImage = newImageReference "01ARZ3NDEKTSV4RRFFQ69G5FAW"

input :: DraftInput
input = DraftInput "Haskell" "Body with managed-image" (Just "haskell-syntax") ["01ARZ3NDEKTSV4RRFFQ69G5FAY"]

-- A deterministic domain-test port, not a Markdown parser.
extractImages :: ExtractImageReferences
extractImages body
    | "managed-image" `Text.isInfixOf` draftBodyText body = Set.singleton <$> image
    | otherwise = Right Set.empty

confirmed :: Either DomainError AvailableImageReferences
confirmed = do
    reference <- image
    let references = Set.singleton reference
    confirmAvailableImageReferences references references

emptyContent :: Either DomainError DraftContent
emptyContent = newDraftContent extractImages (DraftInput "Idea" "" Nothing [])

timestamp :: Integer -> UTCTime
timestamp seconds = addUTCTime (fromInteger seconds) (read "2026-01-01 00:00:00 UTC")

start :: Either DomainError UnvalidatedDraft
start = do
    value <- identifier
    content <- newDraftContent extractImages input
    newUnvalidatedDraft value (timestamp 0) content
