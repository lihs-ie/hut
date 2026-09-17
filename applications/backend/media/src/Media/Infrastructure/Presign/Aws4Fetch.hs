module Media.Infrastructure.Presign.Aws4Fetch (
    newAws4FetchPresigner,
) where

import Control.Exception (SomeException, try)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, addUTCTime)
import GHC.Wasm.Prim (JSVal)
#if defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim (JSString (JSString), fromJSString, toJSString)
#endif
import Shared.Domain.Error (DomainError, createServiceUnavailable)
import "media" Media.Domain.Image
import "media" Media.UseCase.RequestImageUpload (
    UploadDestination,
    newUploadDestination,
    newUploadDestinationURL,
 )

#if defined(wasm32_HOST_ARCH)
textToJSVal :: Text.Text -> JSVal
textToJSVal value = case toJSString (Text.unpack value) of
    JSString rawValue -> rawValue

textFromJSVal :: JSVal -> Text.Text
textFromJSVal = Text.pack . fromJSString . JSString
#else
textToJSVal :: Text.Text -> JSVal
textToJSVal _ = error "Aws4Fetch presigning is only executable in a wasm32 Worker"

textFromJSVal :: JSVal -> Text.Text
textFromJSVal _ = error "Aws4Fetch presigning is only executable in a wasm32 Worker"
#endif

newAws4FetchPresigner ::
    JSVal ->
    NominalDiffTime ->
    AwaitingUploadImage ->
    IO (Either DomainError UploadDestination)
newAws4FetchPresigner rawEnvironment lifetime awaiting = do
    signed <- try @SomeException issue
    pure $ case signed of
        Left _ ->
            Left
                ( createServiceUnavailable
                    "ImageUploadDestination"
                    "issue image upload destination failed"
                )
        Right (rawURL, imageIdentifier, attempt, expiresAt) -> do
            destinationURL <- newUploadDestinationURL rawURL
            pure
                ( newUploadDestination
                    imageIdentifier
                    attempt
                    destinationURL
                    expiresAt
                )
  where
    issue = do
        let (imageIdentifier, attempt, declaredType, declaredSha256, requestedAt) =
                foldAwaitingUploadImage
                    ( \identifier uploadAttempt declaration timestamp ->
                        foldImageUploadDeclaration
                            ( \declared _ digest ->
                                ( identifier
                                , uploadAttempt
                                , declaredImageContentTypeText declared
                                , imageSha256Text digest
                                , timestamp
                                )
                            )
                            declaration
                    )
                    awaiting
            expiresAt = addUTCTime lifetime requestedAt
            expiresInSeconds = floor lifetime
            objectKey =
                imageIdentifierText imageIdentifier
                    <> "/"
                    <> uploadAttemptIdentifierText attempt
        rawURL <-
            jsPresignR2Put
                rawEnvironment
                (textToJSVal objectKey)
                (textToJSVal declaredType)
                (textToJSVal declaredSha256)
                expiresInSeconds
        pure
            ( textFromJSVal rawURL
            , imageIdentifier
            , attempt
            , expiresAt
            )

foreign import javascript safe
    """
    (async () => {
      const presignR2Put = $1.__mediaPresignR2Put;
      if (presignR2Put === undefined) {
        throw new TypeError("Media Aws4Fetch presigner adapter is not installed");
      }
      return (
        await presignR2Put({
          objectKey: $2,
          contentType: $3,
          sha256: $4,
          expiresInSeconds: $5
        })
      ).toString();
    })()
    """
    jsPresignR2Put :: JSVal -> JSVal -> JSVal -> JSVal -> Int -> IO JSVal
