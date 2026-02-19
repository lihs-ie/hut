module UseCase.EventHandler
  ( handle,
  )
where

import Aspects.Log (LogEntry (LogEntry), LogLevel (Info))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Writer (MonadWriter (tell))
import Data.List (nubBy)
import Data.Time (getCurrentTime)
import Domain.Common
import Domain.Event (ArticleCreatedPayload (..), ArticleEditedPayload (..), Event (..), EventPayload (..), MemoCreatedPayload (..), MemoEditedPayload (..), MemoEntry (..))
import Domain.Ngram (generateNgramsBySize)
import Domain.SearchToken (ContentType (..), Persist, SearchToken (..), SearchTokenError, TerminateByReference)

data PersistContext = PersistContext
  { contentType :: ContentType,
    reference :: String,
    timeline :: Timeline,
    searchableText :: String,
    tags :: [String]
  }

maxNgramTokens :: Int
maxNgramTokens = 30

type TokenIdentifier = String

type TokenValue = String

generateNgramIdentifiers :: [[String]] -> [(TokenIdentifier, TokenValue)]
generateNgramIdentifiers ngramsBySize =
  let sizeCount = length ngramsBySize
      perSize = if sizeCount == 0 then 0 else maxNgramTokens `div` sizeCount
   in concatMap (take perSize . map (\candidate -> ("ngram:" ++ candidate, candidate))) ngramsBySize

generateTagIdentifiers :: [String] -> [(TokenIdentifier, TokenValue)]
generateTagIdentifiers = map (\candidate -> ("tag:" ++ candidate, candidate))

concatIdentifiers :: [(TokenIdentifier, TokenValue)] -> [(TokenIdentifier, TokenValue)] -> [(TokenIdentifier, TokenValue)]
concatIdentifiers ngrams tags = nubBy (\a b -> fst a == fst b) (ngrams ++ tags)

buildToken :: (TokenIdentifier, TokenValue) -> PersistContext -> SearchToken
buildToken (tokenIdentifier, tokenValue) context =
  SearchToken
    { identifier = tokenIdentifier,
      reference = context.reference,
      contentType = context.contentType,
      value = tokenValue,
      timeline = context.timeline
    }

buildTokens :: PersistContext -> [SearchToken]
buildTokens context =
  let ngrams = generateNgramIdentifiers (generateNgramsBySize 2 4 context.searchableText)
      tags = generateTagIdentifiers context.tags
      tokens = concatIdentifiers ngrams tags
   in map (`buildToken` context) tokens

handle :: (MonadIO m, MonadWriter [LogEntry] m) => Persist m -> TerminateByReference m -> Event -> m (Either SearchTokenError ())
handle persist terminate event = do
  now <- liftIO getCurrentTime
  tell [LogEntry Info ("Handling event: " <> event.identifier) now]
  case event.payload of
    ArticleCreatedPayload' article ->
      persistHandle persist $
        PersistContext
          Article
          article.identifier
          article.timeline
          (article.title ++ article.excerpt ++ article.content)
          article.tags
    ArticleEditedPayload' edited ->
      persistHandle persist $
        PersistContext
          Article
          edited.next.identifier
          edited.next.timeline
          (edited.next.title ++ edited.next.excerpt ++ edited.next.content)
          edited.next.tags
    ArticleTerminatePayload' reference -> terminateHandle terminate (show Article <> ":" <> reference)
    MemoCreatedPayload' memo ->
      persistHandle persist $
        PersistContext
          Memo
          memo.identifier
          memo.timeline
          (memo.title ++ unwords (map (\entry -> entry.text) memo.entries))
          memo.tags
    MemoEditedPayload' edited ->
      persistHandle persist $
        PersistContext
          Memo
          edited.next.identifier
          edited.next.timeline
          (edited.next.title ++ unwords (map (\entry -> entry.text) edited.next.entries))
          edited.next.tags
    MemoTerminatePayload' reference -> terminateHandle terminate (show Memo <> ":" <> reference)

persistHandle :: (Monad m) => Persist m -> PersistContext -> m (Either SearchTokenError ())
persistHandle persist' context = persist' $ buildTokens context

terminateHandle :: (Monad m) => TerminateByReference m -> String -> m (Either SearchTokenError ())
terminateHandle terminate' = terminate'
