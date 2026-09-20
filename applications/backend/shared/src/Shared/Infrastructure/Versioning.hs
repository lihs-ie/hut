{-# LANGUAGE RoleAnnotations #-}

module Shared.Infrastructure.Versioning (
    Version,
    newVersion,
    versionInteger,
    initialVersion,
    nextVersion,
    VersionContext,
    emptyVersionContext,
    PersistenceMode (..),
    observe,
    persistenceMode,
    terminationVersion,
    recordPersisted,
    recordTerminated,
    checkExpectedVersion,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Shared.Domain.Common.Primitive
import Shared.Domain.Error

newtype Version = Version PositiveInteger
    deriving stock (Show, Eq)

data Observation = Missing | Loaded Version | Terminated
    deriving stock (Eq)

type role VersionContext nominal
data VersionContext identifier
    = VersionContext
        Text
        (identifier -> Text)
        (Map identifier Observation)

data PersistenceMode = Insert | Update Version deriving stock (Show, Eq)

newVersion :: PositiveInteger -> Version
newVersion = Version

versionInteger :: Version -> Integer
versionInteger (Version value) = positiveIntegerValue value

initialVersion :: Version
initialVersion = newVersion one

nextVersion :: Version -> Version
nextVersion (Version value) = newVersion (nextPositiveInteger value)

emptyVersionContext :: Text -> (identifier -> Text) -> VersionContext identifier
emptyVersionContext name render = VersionContext name render Map.empty

-- Retain the first observation, except for successful local writes.
-- A local deletion stays a tombstone even after Find returns Nothing.
observe ::
    (Ord identifier) =>
    identifier ->
    Maybe Version ->
    VersionContext identifier ->
    Either DomainError (VersionContext identifier)
observe identifier found context@(VersionContext name render entries) =
    case Map.lookup identifier entries of
        Nothing -> Right (VersionContext name render (Map.insert identifier observation entries))
        Just Terminated | found == Nothing -> Right context
        Just previous | previous == observation -> Right context
        _ -> Left (createProcessingTargetChanged name "the observed aggregate changed")
  where
    observation = maybe Missing Loaded found

persistenceMode ::
    (Ord identifier) => identifier -> VersionContext identifier -> Either DomainError PersistenceMode
persistenceMode identifier (VersionContext _ _ entries) = case Map.lookup identifier entries of
    Nothing -> Right Insert
    Just Missing -> Right Insert
    Just (Loaded version) -> Right (Update version)
    Just Terminated -> Left (createOperationNotAllowed "Persist" "a terminated aggregate cannot be recreated")

terminationVersion ::
    (Ord identifier) => identifier -> VersionContext identifier -> Either DomainError Version
terminationVersion identifier (VersionContext name render entries) = case Map.lookup identifier entries of
    Just (Loaded version) -> Right version
    _ -> Left (createAggregateNotFound name (render identifier))

recordPersisted ::
    (Ord identifier) => identifier -> Version -> VersionContext identifier -> VersionContext identifier
recordPersisted identifier version (VersionContext name render entries) =
    VersionContext name render (Map.insert identifier (Loaded version) entries)

recordTerminated ::
    (Ord identifier) => identifier -> VersionContext identifier -> VersionContext identifier
recordTerminated identifier (VersionContext name render entries) =
    VersionContext name render (Map.insert identifier Terminated entries)

checkExpectedVersion :: Text -> Version -> Version -> Either DomainError ()
checkExpectedVersion name expected actual
    | expected == actual = Right ()
    | otherwise = Left (createProcessingTargetChanged name "the processing target changed")
