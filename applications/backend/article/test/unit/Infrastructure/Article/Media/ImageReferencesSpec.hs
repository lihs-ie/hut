{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.Media.ImageReferencesSpec (run) where

import Control.Monad (unless)
import Data.Set qualified as Set
import Data.Text (Text)
import Infrastructure.Article.Media.ImageReferences (extractManagedImageReferences)
import "article" Domain.Article.Common (imageReferenceText, newDraftBody)
import "shared" Shared.Domain.Error (DomainError (..))

origin :: Text
origin = "https://assets.hut.dev.lihs-dev.com"

identifier :: Text
identifier = "01ARZ3NDEKTSV4RRFFQ69G5FAV"

run :: IO ()
run = do
    let body = newDraftBody
            ("![cover](" <> origin <> "/images/" <> identifier <> ")\n"
                <> "![again](" <> origin <> "/images/" <> identifier <> "?width=320)\n"
                <> "![external](https://example.com/images/other)")
        result = extractManagedImageReferences (origin <> "/") body
    check "managed references are deduplicated" (case result of
        Right references ->
            map imageReferenceText (Set.toList references) == [identifier]
        Left _ -> False)
    check "external image is ignored" (extractManagedImageReferences origin
        (newDraftBody "![external](https://example.com/images/image.png)") == Right Set.empty)
    check "managed URL may end at end of body" (case extractManagedImageReferences origin
        (newDraftBody (origin <> "/images/" <> identifier)) of
        Right references -> map imageReferenceText (Set.toList references) == [identifier]
        Left _ -> False)
    check "empty origin is rejected" (isInvariant
        (extractManagedImageReferences "" body))
    check "invalid image identifier is rejected" (isInvariant
        (extractManagedImageReferences origin
            (newDraftBody (origin <> "/images/not-an-identifier"))))
    check "identifier suffix is not silently truncated" (isInvariant
        (extractManagedImageReferences origin
            (newDraftBody (origin <> "/images/" <> identifier <> "extra"))))

check :: String -> Bool -> IO ()
check label condition = unless condition (fail label)

isInvariant :: Either DomainError value -> Bool
isInvariant (Left (InvariantViolation failure)) = not (null (show failure))
isInvariant _ = False
