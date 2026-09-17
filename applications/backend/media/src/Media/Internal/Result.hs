module Media.Internal.Result (
    ImageUseCase (..),
    ImageEventsFor,
    Result,
    newResult,
    resultOutput,
    resultEvents,
) where

import Data.Kind (Type)
import Media.Domain.Image.Event (ImageBecameAvailable, ImageUploadRejected)
import Shared.Domain.Event (Events)

type data ImageUseCase
    = RequestImageUpload
    | RetryImageUpload
    | GetImageStatus
    | RetryImageInspection
    | InspectImage

type family ImageEventsFor (useCase :: ImageUseCase) :: [Type] where
    ImageEventsFor RequestImageUpload = '[]
    ImageEventsFor RetryImageUpload = '[]
    ImageEventsFor GetImageStatus = '[]
    ImageEventsFor RetryImageInspection = '[]
    ImageEventsFor InspectImage = '[ImageBecameAvailable, ImageUploadRejected]

data Result (useCase :: ImageUseCase) output = Result output (Events (ImageEventsFor useCase))

newResult :: output -> Events (ImageEventsFor useCase) -> Result useCase output
newResult = Result

resultOutput :: Result useCase output -> output
resultOutput (Result output _) = output

resultEvents :: Result useCase output -> Events (ImageEventsFor useCase)
resultEvents (Result _ events) = events
