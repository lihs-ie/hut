module UseCase.Transaction.Fixture where

import Control.Exception (SomeException, try)
import Data.IORef
import Domain.Article
import Domain.Article.Criteria
import Shared.Domain.Common.Transaction
import Shared.Domain.Error
import Shared.Domain.Event (Events (..))
import Shared.Infrastructure.Transaction
import Shared.Infrastructure.Versioning
import Shared.UseCase.Outbox (Append)
import UseCase.Reading (matchesFilter)

data Store = Store {article :: Maybe Article, outbox :: [String]}
    deriving stock (Eq, Show)
data Failure = NoFailure | FailWrite | FailOutbox | FailCommit | UnknownCommit deriving stock (Eq, Show)
data MemoryContext = MemoryContext
    { local :: IORef Store
    , version :: IORef Version
    , observed :: IORef (VersionContext ArticleIdentifier)
    , record :: String -> IO ()
    , failure :: Failure
    }
type TestTx = Transaction MemoryContext IO
data Fixture = Fixture
    { manager :: TransactionManager MemoryContext IO
    , stored :: IORef Store
    , trace :: IORef [String]
    , active :: IORef Bool
    , version :: IORef Version
    }

storageFailure :: DomainError
storageFailure = createServiceUnavailable "TransactionTest" "injected failure"

track :: MemoryContext -> ArticleIdentifier -> Maybe Version -> IO (Either DomainError ())
track context identifier version = do
    entries <- readIORef context.observed
    case observe identifier version entries of
        Left err -> pure (Left err)
        Right updated -> writeIORef context.observed updated >> pure (Right ())

findArticle :: FindArticle TestTx
findArticle identifier = transactionAction $ \context -> do
    context.record "find"
    store <- readIORef context.local
    version <- readIORef context.version
    let found =
            store.article >>= \article ->
                if articleIdentifier article == identifier then Just article else Nothing
    recorded <- track context identifier (version <$ found)
    pure (found <$ recorded)

findExpected :: Version -> FindArticle TestTx
findExpected expected identifier = do
    transactionAction (\context -> context.record "generation" >> pure (Right ()))
    found <- findArticle identifier
    case found of
        Nothing -> pure Nothing
        Just _ -> do
            transactionAction $ \context -> checkExpectedVersion "Article" expected <$> readIORef context.version
            pure found

persistArticle :: PersistArticle TestTx
persistArticle article = transactionAction $ \context -> do
    store <- readIORef context.local
    actualVersion <- readIORef context.version
    entries <- readIORef context.observed
    let identifier = articleIdentifier article
    case persistenceMode identifier entries of
        Left err -> pure (Left err)
        Right mode -> do
            context.record (case mode of Insert -> "insert"; Update _ -> "persist")
            let checked = case mode of
                    Insert -> case store.article of
                        Nothing -> Right initialVersion
                        Just _ -> Left (createOperationNotAllowed "Article" "identifier already exists")
                    Update expected -> case store.article of
                        Just current
                            | articleIdentifier current == identifier ->
                                nextVersion actualVersion <$ checkExpectedVersion "Article" expected actualVersion
                        _ -> Left (createProcessingTargetChanged "Article" "the loaded article no longer exists")
            case checked of
                Left err -> pure (Left err)
                Right version
                    | context.failure == FailWrite -> pure (Left storageFailure)
                    | otherwise -> do
                        writeIORef context.local store{article = Just article}
                        writeIORef context.version version
                        writeIORef context.observed (recordPersisted identifier version entries)
                        pure (Right ())

terminateArticle :: TerminateArticle TestTx
terminateArticle identifier = transactionAction $ \context -> do
    context.record "terminate"
    entries <- readIORef context.observed
    store <- readIORef context.local
    version <- readIORef context.version
    let checked = do
            expected <- terminationVersion identifier entries
            checkExpectedVersion "Article" expected version
            case store.article of
                Just article | articleIdentifier article == identifier -> Right ()
                _ -> Left (createProcessingTargetChanged "Article" "the loaded article no longer exists")
    case checked of
        Left err -> pure (Left err)
        Right ()
            | context.failure == FailWrite -> pure (Left storageFailure)
            | otherwise -> do
                writeIORef context.local store{article = Nothing}
                writeIORef context.observed (recordTerminated identifier entries)
                pure (Right ())

appendEvents :: String -> Append events TestTx
appendEvents label _ (Events events) = transactionAction $ \context -> do
    context.record ("outbox:" <> label)
    if context.failure == FailOutbox
        then pure (Left storageFailure)
        else do
            modifyIORef' context.local (\s -> s{outbox = s.outbox <> replicate (length events) label})
            pure (Right ())

findBySlug :: FindArticleBySlug TestTx
findBySlug slug = transactionAction $ \context -> do
    context.record "slug"
    store <- readIORef context.local
    case store.article of
        Just (Published article) | article.publication.slug == slug -> do
            version <- readIORef context.version
            result <- track context article.identifier (Just version)
            pure (store.article <$ result)
        _ -> pure (Right Nothing)

findSlugOwner :: FindSlugOwner TestTx
findSlugOwner _ = transactionAction $ \context -> do
    context.record "owner"
    Right . fmap articleIdentifier . (.article) <$> readIORef context.local

searchArticles :: SearchArticles TestTx
searchArticles criteria = transactionAction $ \context -> do
    context.record "browse-admin"
    store <- readIORef context.local
    version <- readIORef context.version
    let articles = filter (matchesFilter (status criteria)) (maybe [] pure store.article)
        page = take (pageSize criteria) (drop (pageOffset criteria) articles)
    recorded <- traverse (\a -> track context (articleIdentifier a) (Just version)) page
    pure ((length articles, page) <$ sequence recorded)

searchPublished :: SearchPublishedArticles TestTx
searchPublished criteria = do
    if status criteria /= PublishedOnly
        then abort (createInvariantViolation "Criteria" "reader search requires published articles")
        else pure ()
    transactionAction $ \context -> do
        context.record "browse-reader"
        store <- readIORef context.local
        version <- readIORef context.version
        let articles = case store.article of
                Just (Published article) -> [article]
                _ -> []
            page = take (pageSize criteria) (drop (pageOffset criteria) articles)
        recorded <- traverse (\a -> track context a.identifier (Just version)) page
        pure ((length articles, page) <$ sequence recorded)

-- One-aggregate memory driver. Contract tests only, not a database adapter.
newFixture :: Maybe Article -> Failure -> IO Fixture
newFixture initial failure = do
    stored <- newIORef (Store initial [])
    trace <- newIORef []
    active <- newIORef False
    version <- newIORef initialVersion
    let record action = modifyIORef' trace (<> [action])
        manager = newTransactionManager $ TransactionDriver $ \callback -> do
            alreadyActive <- atomicModifyIORef' active (\current -> (True, current))
            if alreadyActive
                then pure (RolledBack (createOperationNotAllowed "Transaction" "nested execution"))
                else do
                    record "begin"
                    snapshot <- readIORef stored
                    local <- newIORef snapshot
                    localVersion <- readIORef version >>= newIORef
                    observed <- newIORef (emptyVersionContext "Article" articleIdentifierText)
                    attempted <- try (callback (MemoryContext local localVersion observed record failure))
                    let result = case attempted of
                            Left (_ :: SomeException) -> Left storageFailure
                            Right value -> value
                    updated <- readIORef local
                    writeIORef active False
                    case result of
                        Left err -> record "rollback" >> pure (RolledBack err)
                        Right value
                            | failure == FailCommit && updated /= snapshot -> record "rollback" >> pure (RolledBack storageFailure)
                            | otherwise -> do
                                writeIORef stored updated
                                readIORef localVersion >>= writeIORef version
                                if failure == UnknownCommit
                                    then record "unknown" >> pure (OutcomeUnknown "acknowledgement lost")
                                    else record "commit" >> pure (Committed value)
    pure (Fixture manager stored trace active version)
