{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Shared.FFI.SecureRandom (
    secureRandomBytes,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
import Shared.Domain.Error (
    DomainError,
    createInvariantViolation,
    createUnexpectedError,
 )

#ifdef WASI
import Data.Word (Word8)
import Foreign.C.Types (CSize (CSize), CUShort (CUShort))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
#else
import Control.Exception (SomeException, displayException, try)
import System.Entropy qualified as Entropy
#endif

secureRandomBytes :: Int -> IO (Either DomainError ByteString)
secureRandomBytes byteCount
    | byteCount <= 0 = pure (Left invalidByteCountError)
    | otherwise = secureRandomBytesForPlatform byteCount

#ifdef WASI
foreign import ccall unsafe "__wasi_random_get"
    wasiRandomGet :: Ptr Word8 -> CSize -> IO CUShort

secureRandomBytesForPlatform :: Int -> IO (Either DomainError ByteString)
secureRandomBytesForPlatform byteCount =
    allocaBytes byteCount $ \buffer -> do
        CUShort errno <- wasiRandomGet buffer (CSize (fromIntegral byteCount))
        if errno == 0
            then
                Right
                    <$> ByteString.packCStringLen
                        (castPtr buffer, byteCount)
            else
                pure
                    ( Left
                        ( secureRandomFailure
                            ("WASI random_get failed with errno " <> show errno)
                        )
                    )
#else
secureRandomBytesForPlatform :: Int -> IO (Either DomainError ByteString)
secureRandomBytesForPlatform byteCount = do
    result <-
        try (Entropy.getEntropy byteCount)
            :: IO (Either SomeException ByteString)
    pure $ case result of
        Left exception ->
            Left (secureRandomFailure (displayException exception))
        Right bytes
            | ByteString.length bytes == byteCount -> Right bytes
            | otherwise ->
                Left
                    ( secureRandomFailure
                        "the operating system returned an unexpected byte count"
                    )
#endif

invalidByteCountError :: DomainError
invalidByteCountError =
    createInvariantViolation
        "SecureRandom"
        "byte count must be greater than zero"

secureRandomFailure :: String -> DomainError
secureRandomFailure reason =
    createUnexpectedError "SecureRandom" (Text.pack reason)
