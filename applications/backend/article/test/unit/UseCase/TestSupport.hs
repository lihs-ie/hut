{-# LANGUAGE GADTs #-}

module UseCase.TestSupport (
    command,
    expectError,
    references,
    PersistCall,
    recorder,
    checkPersisted,
    amendmentPayload,
) where

import Data.IORef (IORef, modifyIORef', readIORef)
import Domain.Article.Common
import Domain.Article.Draft
import Domain.Article.Event
import Shared.Domain.Error (DomainError)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.UseCase.Command (Command (Command), newActor, newCausation, newCorrelationIdentifier)
import TestSupport (check, right, timestamp)
import UseCase.AmendDraft (AmendDraftPayload (..))
import UseCase.LegacyPersistence (PersistDraft, commandContext)

command :: payload -> IO (Command payload)
command payload = do
    actor <- right (newActor "administrator")
    correlation <- right (newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV")
    cause <- right (newCausation "editor-save")
    pure (Command payload (timestamp 10) actor correlation (Just cause))

expectError :: String -> DomainError -> Either DomainError result -> IO ()
expectError label expected result = check label $ case result of
    Left actual -> actual == expected
    Right _ -> False

references :: Events '[DomainEvent kind ImageReferences] -> IO ImageReferences
references (Events [Here (DomainEvent payload)]) = pure payload
references _ = fail "expected exactly one image reference event"

type PersistCall = (Command (), UnvalidatedDraft, ImageReferences)

recorder ::
    IORef [PersistCall] ->
    Either DomainError () ->
    PersistDraft IO '[DomainEvent kind ImageReferences]
recorder calls outcome context article events = do
    payload <- references events
    modifyIORef' calls (<> [(context, article, payload)])
    pure outcome

checkPersisted ::
    IORef [PersistCall] ->
    Command payload ->
    UnvalidatedDraft ->
    Events '[DomainEvent kind ImageReferences] ->
    IO ()
checkPersisted calls original article events = do
    saved <- readIORef calls
    emitted <- references events
    check
        "one atomic save receives output and identical event"
        (saved == [(commandContext original, article, emitted)])
    check
        "event uses article identity and complete image set"
        ( emitted.article == draftIdentifier article
            && emitted.images == (draftContent article).images
        )

amendmentPayload :: ArticleIdentifier -> DraftInput -> AmendDraftPayload
amendmentPayload article input =
    AmendDraftPayload article input.title input.body input.slug input.tags
