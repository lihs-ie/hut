module Shared.Domain.TagSpec (run) where

import Data.Either (isLeft)
import Shared.Domain.Tag (newTagIdentifier, tagIdentifierText)

run :: IO Bool
run = pure $ case newTagIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAY" of
    Left _ -> False
    Right identifier ->
        tagIdentifierText identifier == "01ARZ3NDEKTSV4RRFFQ69G5FAY"
            && isLeft (newTagIdentifier "haskell")
            && isLeft (newTagIdentifier "01arz3ndektsv4rrffq69g5fay")
