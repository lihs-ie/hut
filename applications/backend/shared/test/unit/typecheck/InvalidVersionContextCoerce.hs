module InvalidVersionContextCoerce where

import Data.Coerce (coerce)
import Shared.Infrastructure.Versioning

newtype ArticleIdentifier = ArticleIdentifier Int deriving (Eq, Ord)
newtype ImageIdentifier = ImageIdentifier Int deriving (Eq, Ord)

invalid :: VersionContext ArticleIdentifier -> VersionContext ImageIdentifier
invalid = coerce
