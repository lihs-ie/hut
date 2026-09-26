{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

-- Feasibility fixture, not the production Article model or TransactionManager.
module Main (main) where

import Control.Exception (SomeException, catch)
import Control.Monad (when)
import GHC.Wasm.Prim (JSVal)

data ProbeError = Rejected

decide :: Int -> Either ProbeError Int
decide value
    | value < 0 = Left Rejected
    | otherwise = Right (value + 1)

workflow :: JSVal -> Int -> IO Int
workflow storage mode = body `catch` failure
  where
    failure :: SomeException -> IO Int
    failure _ = pure (-2)
    body = do
        current <- readRevision storage
        when (mode == 7) pause
        case decide current of
            Left _ -> pure (-1)
            Right next -> do
                updateRevision storage next
                observed <- readRevision storage
                if observed /= next
                    then pure (-3)
                    else if mode == 1
                        then pure (-1)
                        else do
                            succeeded <- appendEvent storage next
                            when (not succeeded) (ioError (userError "SQL failure"))
                            when (mode == 2) $ do
                                duplicate <- appendEvent storage next
                                when (not duplicate) (ioError (userError "duplicate event"))
                            when (mode == 3) (ioError (userError "Haskell exception"))
                            pure next

initialize :: JSVal -> IO ()
initialize = initializeSQL

snapshot :: JSVal -> IO JSVal
snapshot = snapshotSQL

foreign export javascript "workflow" workflow :: JSVal -> Int -> IO Int
foreign export javascript "initialize" initialize :: JSVal -> IO ()
foreign export javascript "snapshot" snapshot :: JSVal -> IO JSVal

foreign import javascript unsafe
    "$1.sql.exec('SELECT revision FROM article WHERE identifier = 1').one().revision"
    readRevision :: JSVal -> IO Int

foreign import javascript unsafe
    "$1.sql.exec('UPDATE article SET revision = ? WHERE identifier = 1', $2)"
    updateRevision :: JSVal -> Int -> IO ()

foreign import javascript unsafe
    "(() => { try { $1.sql.exec('INSERT INTO outbox (identifier) VALUES (?)', $2); return true; } catch (_) { return false; } })()"
    appendEvent :: JSVal -> Int -> IO Bool

foreign import javascript safe
    "new Promise(resolve => setTimeout(resolve, 10))"
    pause :: IO ()

foreign import javascript unsafe
    "(() => { $1.sql.exec('CREATE TABLE IF NOT EXISTS article (identifier INTEGER PRIMARY KEY, revision INTEGER NOT NULL)'); $1.sql.exec('CREATE TABLE IF NOT EXISTS outbox (identifier INTEGER PRIMARY KEY)'); $1.sql.exec('INSERT OR IGNORE INTO article VALUES (1, 0)'); })()"
    initializeSQL :: JSVal -> IO ()

foreign import javascript unsafe
    "({revision: $1.sql.exec('SELECT revision FROM article WHERE identifier = 1').one().revision, events: $1.sql.exec('SELECT count(*) AS n FROM outbox').one().n})"
    snapshotSQL :: JSVal -> IO JSVal

main :: IO ()
main = pure ()
