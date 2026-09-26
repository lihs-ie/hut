module Shared.Domain.Identifier (
    ULID,
    newULID,
    newULIDFromParts,
    ulidText,
    ulidFromInteger,
    ulidToInteger,
) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Shared.Domain.Error (DomainError, createInvariantViolation)

newtype ULID = ULID Integer
    deriving stock (Eq, Ord)

instance Show ULID where
    show = Text.unpack . ulidText

newULID :: Text -> Either DomainError ULID
newULID value
    | Text.length value /= encodedLength = Left canonicalULIDError
    | not (hasCanonicalPrefix value) = Left canonicalULIDError
    | Text.any (not . isCanonicalCharacter) value = Left canonicalULIDError
    | otherwise = Right (ULID (decodeBase32 value))

ulidText :: ULID -> Text
ulidText (ULID value) =
    Text.pack
        [ Text.index crockfordAlphabet (digitAt position)
        | position <- [encodedLength - 1, encodedLength - 2 .. 0]
        ]
  where
    digitAt position =
        fromInteger ((value `div` (base ^ position)) `mod` base)

ulidFromInteger :: Integer -> Either DomainError ULID
ulidFromInteger value
    | value < 0 || value > maxULID = Left integerRangeError
    | otherwise = Right (ULID value)

newULIDFromParts :: UTCTime -> ByteString -> Either DomainError ULID
newULIDFromParts timestamp entropy
    | ByteString.length entropy /= entropyLength = Left entropyLengthError
    | timestampMilliseconds < 0 || timestampMilliseconds > maxTimestamp =
        Left timestampRangeError
    | otherwise =
        Right
            ( ULID
                ( (timestampMilliseconds `shiftL` entropyBitLength)
                    .|. entropyInteger
                )
            )
  where
    timestampMilliseconds =
        floor (utcTimeToPOSIXSeconds timestamp * 1000)
    entropyInteger =
        ByteString.foldl'
            (\value byte -> (value `shiftL` 8) .|. toInteger byte)
            0
            entropy

ulidToInteger :: ULID -> Integer
ulidToInteger (ULID value) = value

decodeBase32 :: Text -> Integer
decodeBase32 = Text.foldl' step 0
  where
    step accumulator character =
        accumulator * base + toInteger (alphabetIndex character)

alphabetIndex :: Char -> Int
alphabetIndex character =
    case Text.findIndex (== character) crockfordAlphabet of
        Just index -> index
        Nothing -> 0

isCanonicalCharacter :: Char -> Bool
isCanonicalCharacter character = Text.any (== character) crockfordAlphabet

hasCanonicalPrefix :: Text -> Bool
hasCanonicalPrefix value =
    case Text.uncons value of
        Just (firstCharacter, _) -> firstCharacter >= '0' && firstCharacter <= '7'
        Nothing -> False

crockfordAlphabet :: Text
crockfordAlphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

base :: Integer
base = 32

encodedLength :: Int
encodedLength = 26

maxTimestamp :: Integer
maxTimestamp = (2 ^ (48 :: Int)) - 1

entropyLength :: Int
entropyLength = 10

entropyBitLength :: Int
entropyBitLength = entropyLength * 8

maxULID :: Integer
maxULID = (2 ^ (128 :: Int)) - 1

canonicalULIDError :: DomainError
canonicalULIDError =
    createInvariantViolation
        "ULID"
        "value must be a canonical Crockford Base32 ULID"

integerRangeError :: DomainError
integerRangeError =
    createInvariantViolation
        "ULID"
        "value must be within the unsigned 128-bit range"

entropyLengthError :: DomainError
entropyLengthError =
    createInvariantViolation
        "ULID"
        "entropy must contain exactly 10 bytes"

timestampRangeError :: DomainError
timestampRangeError =
    createInvariantViolation
        "ULID"
        "timestamp must be within the unsigned 48-bit millisecond range"
