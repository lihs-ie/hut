{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.DurableObject.LogOutbox (
    dispatchPendingLogs,
    dispatchPendingLogsWith,
) where

import Cloudflare.Workers.Binding.DurableObject (DurableObjectStorage)
import Cloudflare.Workers.Binding.DurableObject.SQL (
    SQLError,
    SQLLimits (..),
    SQLResult (..),
    SQLStatement (..),
    SQLValue (..),
    sqlExec,
 )
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Aeson (Value, eitherDecodeStrict')
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Infrastructure.Article.DurableObject.Outbox (OutboxDispatchResult (..))
import Infrastructure.Article.DurableObject.Repository (ExecuteSQL)
import "shared" Shared.Domain.Error (
    DomainError,
    createServiceUnavailable,
    createUnexpectedError,
 )

data PendingLog = PendingLog
    { identifier :: Text
    , payload :: Text
    }

type WriteLog = Text -> IO ()

batchSize :: Int
batchSize = 100

sqlLimits :: SQLLimits
sqlLimits = SQLLimits{maximumRows = batchSize, maximumBytes = 16777216, maximumStatements = 1}

dispatchPendingLogs ::
    DurableObjectStorage -> IO (Either DomainError OutboxDispatchResult)
dispatchPendingLogs storage = dispatchPendingLogsWith (sqlExec storage sqlLimits) (putStrLn . Text.unpack)

dispatchPendingLogsWith ::
    ExecuteSQL -> WriteLog -> IO (Either DomainError OutboxDispatchResult)
dispatchPendingLogsWith execute writeLog = do
    selected <- executeSQL execute
        ("SELECT identifier, payload FROM article_outbox WHERE status = 'pending' "
            <> "AND event_kind IN (?, ?, ?) ORDER BY rowid LIMIT ?")
        (map SQLText logKinds <> [SQLNumber (fromIntegral batchSize)])
    case selected >>= traverse readLog . (.rows) of
        Left err -> pure (Left err)
        Right rows -> deliver execute writeLog 0 rows

deliver ::
    ExecuteSQL -> WriteLog -> Int -> [PendingLog] ->
    IO (Either DomainError OutboxDispatchResult)
deliver execute _ count [] = do
    pending <- hasPending execute
    pure $ (\remaining -> OutboxDispatchResult count remaining Nothing) <$> pending
deliver execute writeLog count (row : rest) =
    case eitherDecodeStrict' (encodeUtf8 row.payload) :: Either String Value of
        Left _ -> pure (Right (OutboxDispatchResult count True (Just (corrupt "invalid log payload"))))
        Right _ -> do
            attempted <- try @SomeException (writeLog row.payload)
            case attempted of
                Left exception
                    | Just asynchronous <- fromException @SomeAsyncException exception ->
                        throwIO asynchronous
                    | otherwise ->
                        pure (Right (OutboxDispatchResult count True
                            (Just (createServiceUnavailable "ArticleEventLog" "write failed"))))
                Right () -> do
                    marked <- executeSQL execute
                        ("UPDATE article_outbox SET status = 'delivered', attempts = attempts + 1 "
                            <> "WHERE identifier = ? AND status = 'pending' RETURNING identifier")
                        [SQLText row.identifier]
                    case marked >>= expectMarked row.identifier of
                        Left err -> pure (Right (OutboxDispatchResult count True (Just err)))
                        Right () -> deliver execute writeLog (count + 1) rest

readLog :: [SQLValue] -> Either DomainError PendingLog
readLog [SQLText identifier, SQLText payload]
    | not (Text.null identifier) = Right (PendingLog identifier payload)
readLog _ = Left (corrupt "invalid log row")

expectMarked :: Text -> SQLResult -> Either DomainError ()
expectMarked identifier result = case result.rows of
    [[SQLText actual]] | actual == identifier -> Right ()
    _ -> Left (corrupt "log update did not affect one pending row")

hasPending :: ExecuteSQL -> IO (Either DomainError Bool)
hasPending execute = do
    selected <- executeSQL execute
        "SELECT identifier FROM article_outbox WHERE status = 'pending' AND event_kind IN (?, ?, ?) LIMIT 1"
        (map SQLText logKinds)
    pure $ selected >>= \result -> case result.rows of
        [] -> Right False
        [[SQLText _]] -> Right True
        _ -> Left (corrupt "invalid log pending check")

logKinds :: [Text]
logKinds = ["ArticleReadyToPublish", "ArticlePublished", "ArticleTakenDown"]

executeSQL :: ExecuteSQL -> Text -> [SQLValue] -> IO (Either DomainError SQLResult)
executeSQL execute statement parameters = do
    result <- try (execute SQLStatement{sql = statement, parameters})
    pure $ case result of
        Left (failure :: SQLError) ->
            Left (createServiceUnavailable "ArticleEventLog" (Text.pack (show failure)))
        Right value -> Right value

corrupt :: Text -> DomainError
corrupt = createUnexpectedError "ArticleEventLog"
