module Shared.FFI.SecureRandomSpec (run) where

import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Shared.FFI.SecureRandom qualified as SecureRandom

run :: IO Bool
run = do
    secureEntropyResult <- SecureRandom.secureRandomBytes 10
    invalidEntropyResult <- SecureRandom.secureRandomBytes 0
    pure
        ( secureEntropyHasRequestedLength secureEntropyResult
            && isLeft invalidEntropyResult
        )

secureEntropyHasRequestedLength :: Either error ByteString.ByteString -> Bool
secureEntropyHasRequestedLength result =
    case result of
        Right entropy -> ByteString.length entropy == 10
        Left _ -> False
