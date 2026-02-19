module Domain.SearchToken
  ( ContentType (..),
    SearchToken (..),
    SearchTokenError (..),
    Persist,
    TerminateByReference,
  )
where

import Domain.Common

data ContentType = Article | Memo deriving (Eq)

instance Show ContentType where
  show Article = "article"
  show Memo = "memo"

type Reference = String

data SearchToken = SearchToken
  { identifier :: String,
    reference :: Reference,
    contentType :: ContentType,
    value :: String,
    timeline :: Timeline
  }
  deriving (Eq, Show)

data SearchTokenError = NotFound | Unexpected deriving (Eq, Show)

type Persist m = [SearchToken] -> m (Either SearchTokenError ())

type TerminateByReference m = Reference -> m (Either SearchTokenError ())
