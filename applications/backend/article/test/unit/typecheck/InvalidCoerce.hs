module InvalidCoerce where

import Data.Coerce (coerce)
import Domain.Article.Draft (UnvalidatedDraft, ReadyToPublish)

invalid :: UnvalidatedDraft -> ReadyToPublish
invalid = coerce
