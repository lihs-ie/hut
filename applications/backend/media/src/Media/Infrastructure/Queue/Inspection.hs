module Media.Infrastructure.Queue.Inspection (
    newInspectionEnqueuer,
) where

import Cloudflare.Workers.Binding.Queue (
    QueueBody (QueueJSONBody),
    QueueProducer,
    queueSendDefaultOptions,
    queueSendValue,
 )
import Control.Exception (SomeException, try)
import Data.Aeson (encode, object, (.=))
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text.Encoding qualified as TextEncoding
import Shared.Domain.Error (DomainError, createServiceUnavailable)
import "media" Media.Domain.Image (
    UploadAttemptIdentifier,
    uploadAttemptIdentifierText,
 )

newInspectionEnqueuer ::
    QueueProducer ->
    UploadAttemptIdentifier ->
    IO (Either DomainError ())
newInspectionEnqueuer queue attempt = do
    first
        ( const
            ( createServiceUnavailable
                "MediaInspectionQueue"
                "enqueue image inspection failed"
            )
        )
        <$> try @SomeException send
  where
    send = do
        _ <-
            queueSendValue
                queue
                (QueueJSONBody queueBody)
                queueSendDefaultOptions
        pure ()
    queueBody =
        TextEncoding.decodeUtf8
            . LazyByteString.toStrict
            $ encode
                ( object
                    [ "data"
                        .= object
                            [ "key"
                                .= uploadAttemptIdentifierText attempt
                            ]
                    ]
                )
