module Shared.Transaction.Support where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.IORef
import Shared.Domain.Common.Transaction
import Shared.Domain.Error
import Shared.Infrastructure.Transaction

data Store = Store {article :: Maybe Int, outbox :: [Int]}
    deriving stock (Show, Eq)
data Mode = Normal | BeginFailure | CommitFailure | UnknownBefore | UnknownAfter
    deriving stock (Eq)
data TestContext = TestContext {local :: IORef Store, record :: String -> IO ()}
data Fixture = Fixture
    { manager :: TransactionManager TestContext IO
    , stored :: IO Store
    , trace :: IO [String]
    }

businessError, databaseError :: DomainError
businessError = createOperationNotAllowed "Article" "invalid transition"
databaseError = createServiceUnavailable "Database" "operation failed"

check :: String -> Bool -> IO ()
check name passed = unless passed (fail name)

findArticle :: Transaction TestContext IO (Maybe Int)
findArticle = transactionAction $ \context -> do
    context.record "read"
    Right . (.article) <$> readIORef context.local

persistArticle :: Int -> Transaction TestContext IO ()
persistArticle value = transactionAction $ \context -> do
    context.record "persist"
    modifyIORef' context.local (\s -> s{article = Just value})
    pure (Right ())

appendOutbox :: Int -> Transaction TestContext IO ()
appendOutbox value = transactionAction $ \context -> do
    context.record "outbox"
    modifyIORef' context.local (\s -> s{outbox = s.outbox <> [value]})
    pure (Right ())

failDatabase, nativeFailure :: Transaction TestContext IO ()
failDatabase = transactionAction $ \context -> context.record "db-failure" >> pure (Left databaseError)
nativeFailure = transactionAction $ \context -> context.record "native-failure" >> ioError (userError "native failure")

program :: Transaction TestContext IO Int
program = do
    value <- findArticle
    next <- fromEither $ maybe (Left businessError) (Right . (+ 1)) value
    persistArticle next
    appendOutbox next
    pure next

newFixture :: Mode -> IO Fixture
newFixture mode = do
    store <- newIORef (Store (Just 0) [])
    logRef <- newIORef []
    let record message = modifyIORef' logRef (<> [message])
        driver = TransactionDriver $ \callback -> do
            record "begin"
            if mode == BeginFailure
                then pure (RolledBack databaseError)
                else do
                    snapshot <- readIORef store
                    local <- newIORef snapshot
                    attempted <- try (callback (TestContext local record))
                    let result = case attempted of
                            Left (_ :: SomeException) -> Left databaseError
                            Right value -> value
                    case result of
                        Left err -> record "rollback" >> pure (RolledBack err)
                        Right value
                            | mode == CommitFailure -> record "rollback" >> pure (RolledBack databaseError)
                            | mode == UnknownBefore -> record "unknown" >> pure (OutcomeUnknown "commit acknowledgement lost")
                            | otherwise -> do
                                readIORef local >>= writeIORef store
                                if mode == UnknownAfter
                                    then record "unknown" >> pure (OutcomeUnknown "commit acknowledgement lost")
                                    else record "commit" >> pure (Committed value)
    pure (Fixture (newTransactionManager driver) (readIORef store) (readIORef logRef))
