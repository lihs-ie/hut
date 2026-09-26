module Shared.Domain.DevelopLanguage (DevelopmentLanguage (..)) where

data DevelopmentLanguage
    = Haskell
    | TypeScript
    | Go
    | Rust
    | MoonBit
    | NextJs
    deriving stock (Show, Eq)
