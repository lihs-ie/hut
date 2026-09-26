{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.Media.ImageReferences (
    extractManagedImageReferences,
) where

import Data.Char (isAlphaNum)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import "article" Domain.Article.Common (
    DraftBody,
    ImageReference,
    draftBodyText,
    newImageReference,
 )
import "shared" Shared.Domain.Error (DomainError, createInvariantViolation)

extractManagedImageReferences ::
    Text -> DraftBody -> Either DomainError (Set ImageReference)
extractManagedImageReferences origin body
    | Text.null normalizedOrigin =
        Left (createInvariantViolation "MediaAssetOrigin" "asset origin is required")
    | otherwise = go Set.empty (draftBodyText body)
  where
    normalizedOrigin = Text.dropWhileEnd (== '/') origin
    prefix = normalizedOrigin <> "/images/"

    go found remaining =
        case Text.breakOn prefix remaining of
            (_, suffix) | Text.null suffix -> Right found
            (_, suffix) -> do
                let afterPrefix = Text.drop (Text.length prefix) suffix
                    rawIdentifier = Text.take 26 afterPrefix
                    afterIdentifier = Text.drop 26 afterPrefix
                if Text.length rawIdentifier /= 26 || invalidContinuation afterIdentifier
                    then Left invalidReference
                    else do
                        reference <- either (const (Left invalidReference)) Right
                            (newImageReference rawIdentifier)
                        go (Set.insert reference found) afterIdentifier

    invalidContinuation value = case Text.uncons value of
        Nothing -> False
        Just (character, _) -> isAlphaNum character || character == '-' || character == '_'

    invalidReference =
        createInvariantViolation "ImageReference" "managed asset URL has an invalid identifier"
