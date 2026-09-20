module InvalidVersionContext where

import Shared.Infrastructure.Versioning

newtype ArticleIdentifier = ArticleIdentifier Int deriving (Eq, Ord)
newtype ImageIdentifier = ImageIdentifier Int deriving (Eq, Ord)

invalid :: VersionContext ArticleIdentifier -> VersionContext ImageIdentifier
invalid = recordPersisted (ImageIdentifier 1) initialVersion
