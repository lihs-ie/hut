{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Firestore
  ( createPersist,
    createTerminate,
  )
where

import Aspects.Log (LogEntry (LogEntry), LogLevel (Error, Info))
import Control.Exception (SomeException, fromException, try)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Writer (MonadIO (liftIO), MonadTrans (lift), MonadWriter (tell), WriterT)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Domain.Common (Timeline (createdAt, updatedAt))
import Domain.SearchToken (Persist, SearchToken (contentType, identifier, reference, timeline, value), SearchTokenError (..), TerminateByReference)
import Gogol (AllowRequest, DateTime (..), Env, Error (..), GoogleRequest (Rs), send, _serviceBody, _serviceStatus)
import Gogol.FireStore
import Network.HTTP.Types qualified as HTTP

runFirestore :: (GoogleRequest a, AllowRequest a scopes) => Env scopes -> a -> IO (Rs a)
runFirestore environment request = runResourceT (send environment request)

formatException :: SomeException -> String
formatException exception =
  case fromException exception of
    Just (ServiceError serviceError) ->
      let HTTP.Status code message = _serviceStatus serviceError
          body = maybe "" LBS.unpack (_serviceBody serviceError)
       in "[" <> show code <> " " <> BS.unpack message <> "] " <> body
    Just (TransportError httpException) ->
      "TransportError: " <> show httpException
    Just (SerializeError serializeError) ->
      "SerializeError: " <> show serializeError
    Nothing ->
      show exception

databasePath :: String -> T.Text
databasePath projectID = T.pack ("projects/" <> projectID <> "/databases/(default)")

basePath :: String -> String
basePath projectID = "projects/" <> projectID <> "/databases/(default)/documents"

searchTokenPath :: String -> String -> String
searchTokenPath projectID tokenID = basePath projectID <> "/search-tokens/" <> tokenID

contentTokenIndexPath :: String -> String -> String
contentTokenIndexPath projectID key = basePath projectID <> "/content-token-index/" <> key

referencePath :: String -> String -> String -> String
referencePath projectID tokenID reference = searchTokenPath projectID tokenID <> "/refs/" <> reference

mkStringValue :: T.Text -> Value
mkStringValue text = newValue {stringValue = Just text}

mkTimestampValue :: UTCTime -> Value
mkTimestampValue time = newValue {timestampValue = Just (DateTime time)}


toDocument :: String -> SearchToken -> Document
toDocument projectID token =
  let path = searchTokenPath projectID token.identifier
      tokenType = takeWhile (/= ':') token.identifier
      fieldMap =
        HashMap.fromList
          [ ("identifier", mkStringValue (T.pack token.identifier)),
            ("type", mkStringValue (T.pack tokenType)),
            ("value", mkStringValue (T.pack token.value)),
            ("createdAt", mkTimestampValue token.timeline.createdAt),
            ("updatedAt", mkTimestampValue token.timeline.updatedAt)
          ]
   in Document
        { name = Just (T.pack path),
          fields = Just (Document_Fields fieldMap),
          createTime = Nothing,
          updateTime = Nothing
        }

contentKey :: SearchToken -> String
contentKey token = show token.contentType <> ":" <> token.reference

mkDoubleValue :: Double -> Value
mkDoubleValue number = newValue {doubleValue = Just number}

mkMapValue :: [(T.Text, Value)] -> Value
mkMapValue pairs =
  newValue
    { mapValue =
        Just
          MapValue {fields = Just (MapValue_Fields (HashMap.fromList pairs))}
    }

mkArrayValue :: [T.Text] -> Value
mkArrayValue values =
  newValue
    { arrayValue =
        Just
          ArrayValue {values = Just (map mkStringValue values)}
    }

calculateScore :: SearchToken -> Double
calculateScore token =
  let tokenType = takeWhile (/= ':') token.identifier
   in if tokenType == "tag" then 10.0 else fromIntegral (length token.value)

toRefDocument :: String -> SearchToken -> Document
toRefDocument projectID token =
  let key = contentKey token
      path = referencePath projectID token.identifier key
      identifierValue =
        mkMapValue
          [ ("type", mkStringValue (T.pack (show token.contentType))),
            ("content", mkStringValue (T.pack token.reference))
          ]
      fieldMap =
        HashMap.fromList
          [ ("identifier", identifierValue),
            ("score", mkDoubleValue (calculateScore token)),
            ("updatedAt", mkTimestampValue token.timeline.updatedAt)
          ]
   in Document
        { name = Just (T.pack path),
          fields = Just (Document_Fields fieldMap),
          createTime = Nothing,
          updateTime = Nothing
        }

toIndexDocument :: String -> [SearchToken] -> Maybe Document
toIndexDocument _projectID [] = Nothing
toIndexDocument projectID tokens@(first : _rest) =
  let key = contentKey first
      path = contentTokenIndexPath projectID key
      tokenIdentifiers = map (\t -> T.pack t.identifier) tokens
      fieldsMap =
        HashMap.fromList
          [ ("tokens", mkArrayValue tokenIdentifiers)
          ]
   in Just
        Document
          { name = Just (T.pack path),
            fields = Just (Document_Fields fieldsMap),
            createTime = Nothing,
            updateTime = Nothing
          }

mkUpdateWrite :: Document -> Write
mkUpdateWrite document =
  Write
    { currentDocument = Nothing,
      delete = Nothing,
      transform = Nothing,
      update = Just document,
      updateMask = Nothing,
      updateTransforms = Nothing
    }

executeFirestore ::
  (GoogleRequest a, AllowRequest a scopes) =>
  Env scopes ->
  a ->
  SearchTokenError ->
  String ->
  UTCTime ->
  ExceptT SearchTokenError (WriterT [LogEntry] IO) (Rs a)
executeFirestore environment request onError errorPrefix now = do
  result <- liftIO (try (runFirestore environment request))
  case result of
    Left (exception :: SomeException) -> do
      lift $ tell [LogEntry Error (errorPrefix <> formatException exception) now]
      throwError onError
    Right value -> pure value

createPersist :: (AllowRequest FireStoreProjectsDatabasesDocumentsBatchWrite scopes) => Env scopes -> String -> Persist (WriterT [LogEntry] IO)
createPersist environment projectID tokens = runExceptT $ do
  now <- liftIO getCurrentTime
  let tokenWrites = map (mkUpdateWrite . toDocument projectID) tokens
      refWrites = map (mkUpdateWrite . toRefDocument projectID) tokens
      indexWrite = maybe [] (\d -> [mkUpdateWrite d]) (toIndexDocument projectID tokens)
      allWrites = tokenWrites <> refWrites <> indexWrite
      request =
        BatchWriteRequest
          { labels = Nothing,
            writes = Just allWrites
          }
      batchRequest =
        newFireStoreProjectsDatabasesDocumentsBatchWrite
          (databasePath projectID)
          request
  lift $ tell [LogEntry Info ("Persisting " <> show (length allWrites) <> " writes") now]
  _ <- executeFirestore environment batchRequest Unexpected "BatchWrite failed in persist: " now
  lift $ tell [LogEntry Info "BatchWrite succeeded in persist" now]

extractTokenIdentifiers :: Document -> [T.Text]
extractTokenIdentifiers document =
  let Document_Fields fieldMap = fromMaybe (Document_Fields HashMap.empty) document.fields
      tokenIdentifierValues = HashMap.lookup "tokens" fieldMap
      extractArray v = case v.arrayValue of
        Just (ArrayValue (Just vs)) -> mapMaybe (\item -> item.stringValue) vs
        _ -> []
   in maybe [] extractArray tokenIdentifierValues

mkDeleteWrite :: T.Text -> Write
mkDeleteWrite path =
  Write
    { currentDocument = Nothing,
      delete = Just path,
      transform = Nothing,
      update = Nothing,
      updateMask = Nothing,
      updateTransforms = Nothing
    }

createTerminate ::
  ( AllowRequest FireStoreProjectsDatabasesDocumentsGet scopes,
    AllowRequest FireStoreProjectsDatabasesDocumentsBatchWrite scopes
  ) =>
  Env scopes -> String -> TerminateByReference (WriterT [LogEntry] IO)
createTerminate environment projectID reference = runExceptT $ do
  now <- liftIO getCurrentTime
  let indexPath = contentTokenIndexPath projectID reference
      getRequest = newFireStoreProjectsDatabasesDocumentsGet (T.pack indexPath)
  lift $ tell [LogEntry Info ("Fetching index for reference: " <> reference) now]
  indexDocument <- executeFirestore environment getRequest NotFound ("Index fetch failed for reference: " <> reference <> " - ") now
  let tokenIdentifiers = extractTokenIdentifiers indexDocument
      tokenDeletes = map (mkDeleteWrite . T.pack . searchTokenPath projectID . T.unpack) tokenIdentifiers
      refDeletes = map (\identifier -> mkDeleteWrite (T.pack (referencePath projectID (T.unpack identifier) reference))) tokenIdentifiers
      indexDeletes = [mkDeleteWrite (T.pack indexPath)]
      allDeletes = tokenDeletes <> refDeletes <> indexDeletes
      request =
        BatchWriteRequest
          { labels = Nothing,
            writes = Just allDeletes
          }
      batchRequest =
        newFireStoreProjectsDatabasesDocumentsBatchWrite
          (databasePath projectID)
          request
  lift $ tell [LogEntry Info ("Deleting " <> show (length allDeletes) <> " documents") now]
  _ <- executeFirestore environment batchRequest Unexpected "BatchWrite failed in terminate: " now
  lift $ tell [LogEntry Info "Terminate succeeded" now]
