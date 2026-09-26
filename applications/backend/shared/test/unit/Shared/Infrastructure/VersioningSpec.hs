module Shared.Infrastructure.VersioningSpec (run) where

import Control.Monad (forM_, unless)
import Data.Text (Text)
import Shared.Domain.Common.Primitive
import Shared.Domain.Error
import Shared.Infrastructure.Versioning

checkFailure :: DomainError -> Either DomainError a -> IO ()
checkFailure expected result = check "version contract rejects invalid operation" $ case result of
    Left actual -> actual == expected
    Right _ -> False

run :: IO Bool
run = do
    let identifier = TestIdentifier "one"
        other = TestIdentifier "two"
    let missingError = createAggregateNotFound "Series" (renderIdentifier identifier)
        changedError = createProcessingTargetChanged "Series" "the observed aggregate changed"
        terminatedError = createOperationNotAllowed "Persist" "a terminated aggregate cannot be recreated"
    check "unobserved is insert" (persistenceMode identifier emptyContext == Right Insert)
    checkFailure missingError (terminationVersion identifier emptyContext)
    missing <- right (observe identifier Nothing emptyContext)
    missingAgain <- right (observe identifier Nothing missing)
    check "absent remains insert" (persistenceMode identifier missingAgain == Right Insert)
    checkFailure changedError (observe identifier (Just initialVersion) missing)
    loaded <- right (observe identifier (Just initialVersion) emptyContext)
    same <- right (observe identifier (Just initialVersion) loaded)
    check "same version retained" (persistenceMode identifier same == Right (Update initialVersion))
    check "termination uses observed version" (terminationVersion identifier same == Right initialVersion)
    forM_ [Nothing, Just (nextVersion initialVersion)] $ \observed ->
        checkFailure changedError (observe identifier observed loaded)
    check "expected version matches" (checkExpectedVersion "Series" initialVersion initialVersion == Right ())
    checkFailure
        (createProcessingTargetChanged "Series" "the processing target changed")
        (checkExpectedVersion "Series" initialVersion (nextVersion initialVersion))
    let updated = recordPersisted identifier (nextVersion initialVersion) loaded
    check "own write advances version" (persistenceMode identifier updated == Right (Update (nextVersion initialVersion)))
    check "other identity independent" (persistenceMode other updated == Right Insert)
    let terminated = recordTerminated identifier updated
    checkFailure terminatedError (persistenceMode identifier terminated)
    checkFailure missingError (terminationVersion identifier terminated)
    deleted <- right (observe identifier Nothing terminated)
    checkFailure terminatedError (persistenceMode identifier deleted)
    checkFailure changedError (observe identifier (Just initialVersion) terminated)

    restored <- right (newPositiveInteger 42)
    let version = newVersion restored
    check "restored version" (versionInteger version == 42)
    check "successor" (versionInteger (nextVersion version) == 43)
    check "initial version" (versionInteger initialVersion == 1)
    check
        "different renderers cannot alias keys"
        ( persistenceMode
            other
            ( recordPersisted
                identifier
                version
                (emptyVersionContext "Memo" (const "same"))
            )
            == Right Insert
        )
    checkFailure
        (createAggregateNotFound "Memo" "two")
        (terminationVersion other (emptyVersionContext "Memo" renderIdentifier))
    checkFailure
        (createProcessingTargetChanged "Memo" "the processing target changed")
        (checkExpectedVersion "Memo" initialVersion version)
    pure True

newtype TestIdentifier = TestIdentifier Text deriving stock (Eq, Ord)

renderIdentifier :: TestIdentifier -> Text
renderIdentifier (TestIdentifier value) = value

emptyContext :: VersionContext TestIdentifier
emptyContext = emptyVersionContext "Series" renderIdentifier

check :: String -> Bool -> IO ()
check label passed = unless passed (fail label)

right :: (Show e) => Either e a -> IO a
right = either (fail . show) pure
