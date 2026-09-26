module Media.Domain.Image.Event (
    ImageEventKind (..),
    ImageBecameAvailable,
    ImageBecameAvailablePayload (..),
    ImageUploadRejected,
    ImageUploadRejectedPayload (..),
    newImageBecameAvailable,
    newImageUploadRejected,
) where

import Media.Domain.Image (AvailableImage, ImageIdentifier, ImageRejection)
import Shared.Domain.Event (DomainEvent (DomainEvent))

data ImageEventKind
    = BecameAvailable
    | UploadRejected

type ImageBecameAvailable = DomainEvent 'BecameAvailable ImageBecameAvailablePayload

newtype ImageBecameAvailablePayload = ImageBecameAvailablePayload
    { image :: AvailableImage
    }
    deriving stock (Show, Eq)

type ImageUploadRejected = DomainEvent 'UploadRejected ImageUploadRejectedPayload

data ImageUploadRejectedPayload = ImageUploadRejectedPayload
    { image :: ImageIdentifier
    , reason :: ImageRejection
    }
    deriving stock (Show, Eq)

newImageBecameAvailable :: AvailableImage -> ImageBecameAvailable
newImageBecameAvailable = DomainEvent . ImageBecameAvailablePayload

newImageUploadRejected :: ImageIdentifier -> ImageRejection -> ImageUploadRejected
newImageUploadRejected image reason = DomainEvent (ImageUploadRejectedPayload image reason)
