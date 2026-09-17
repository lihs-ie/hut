{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Shared.Domain.Event (
    DomainEvent (..),
    OneOf (..),
    Events (..),
) where

import Data.Aeson (ToJSON)
import Data.Kind (Type)
import GHC.Generics (Generic)

newtype DomainEvent (kind :: k) p = DomainEvent
    { payload :: p
    }
    deriving stock (Show, Eq, Generic)

instance (ToJSON p) => ToJSON (DomainEvent kind p)

data OneOf (eventTypes :: [Type]) where
    Here :: event -> OneOf (event ': remainingEvents)
    There :: OneOf remainingEvents -> OneOf (otherEvent ': remainingEvents)

newtype Events (eventTypes :: [Type]) = Events
    { values :: [OneOf eventTypes]
    }
