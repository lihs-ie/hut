module InvalidContentUpdate where

import Domain.Article.Common

invalid :: DraftContent -> DraftContent
invalid content = content {body = newDraftBody "changed without extracting images"}
