module Shared.Domain.IdentifierSpec (run) where

import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Shared.Domain.Identifier (
    newULID,
    newULIDFromParts,
    ulidFromInteger,
    ulidText,
    ulidToInteger,
 )

run :: IO Bool
run =
    pure
        ( and
            [ ulidBoundaryValuesAreValid
            , ulidRejectsOutOfRangeIntegers
            , ulidOfficialExampleRoundTrips
            , ulidIntegerRoundTrips
            , ulidRejectsNonCanonicalText
            , ulidPartsAreCombinedInNetworkOrder
            , ulidAcceptsTimestampBoundaries
            , ulidRejectsInvalidEntropyLengths
            , ulidRejectsOutOfRangeTimestamps
            ]
        )

ulidBoundaryValuesAreValid :: Bool
ulidBoundaryValuesAreValid =
    case (ulidFromInteger 0, ulidFromInteger maxULIDInteger) of
        (Right minimumULID, Right maximumULID) ->
            ulidText minimumULID == "00000000000000000000000000"
                && ulidText maximumULID == "7ZZZZZZZZZZZZZZZZZZZZZZZZZ"
        _ -> False

ulidRejectsOutOfRangeIntegers :: Bool
ulidRejectsOutOfRangeIntegers =
    isLeft (ulidFromInteger (-1))
        && isLeft (ulidFromInteger (maxULIDInteger + 1))

ulidOfficialExampleRoundTrips :: Bool
ulidOfficialExampleRoundTrips =
    case newULID "01ARZ3NDEKTSV4RRFFQ69G5FAV" of
        Right identifier ->
            ulidText identifier == "01ARZ3NDEKTSV4RRFFQ69G5FAV"
                && ulidToInteger identifier
                    == 1777027686520646174104517696511196507
        Left _ -> False

ulidIntegerRoundTrips :: Bool
ulidIntegerRoundTrips =
    all roundTrips [0, 1, 31, 32, 123456789, maxULIDInteger]
  where
    roundTrips value =
        case ulidFromInteger value of
            Right identifier ->
                ulidToInteger identifier == value
                    && newULID (ulidText identifier) == Right identifier
            Left _ -> False

ulidRejectsNonCanonicalText :: Bool
ulidRejectsNonCanonicalText =
    all
        (isLeft . newULID)
        [ ""
        , "01ARZ3NDEKTSV4RRFFQ69G5FA"
        , "01ARZ3NDEKTSV4RRFFQ69G5FAV0"
        , "01arz3ndektsv4rrffq69g5fav"
        , "01ARZ3NDEKTSV4RRFFQ69G5FAI"
        , "8ZZZZZZZZZZZZZZZZZZZZZZZZZ"
        ]

ulidPartsAreCombinedInNetworkOrder :: Bool
ulidPartsAreCombinedInNetworkOrder =
    case newULIDFromParts timestamp entropy of
        Right identifier ->
            ulidToInteger identifier
                == (timestampMilliseconds * (2 ^ (80 :: Int)) + entropyInteger)
        Left _ -> False
  where
    timestampMilliseconds :: Integer
    timestampMilliseconds = 1469918176385
    timestamp =
        posixSecondsToUTCTime
            (fromInteger timestampMilliseconds / 1000)
    entropy = ByteString.pack [0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF, 0x10, 0x32]
    entropyInteger = 0x0123456789ABCDEF1032

ulidRejectsInvalidEntropyLengths :: Bool
ulidRejectsInvalidEntropyLengths =
    isLeft (newULIDFromParts baseTime (ByteString.replicate 9 0))
        && isLeft (newULIDFromParts baseTime (ByteString.replicate 11 0))

ulidAcceptsTimestampBoundaries :: Bool
ulidAcceptsTimestampBoundaries =
    case ( newULIDFromParts epoch validEntropy
         , newULIDFromParts maximumTimestamp validEntropy
         ) of
        (Right minimumTimestampULID, Right maximumTimestampULID) ->
            ulidToInteger minimumTimestampULID == 0
                && ulidToInteger maximumTimestampULID
                    == ((2 ^ (48 :: Int)) - 1) * (2 ^ (80 :: Int))
        _ -> False
  where
    epoch = posixSecondsToUTCTime 0
    maximumTimestamp =
        posixSecondsToUTCTime
            (fromInteger ((2 ^ (48 :: Int)) - 1) / 1000)
    validEntropy = ByteString.replicate 10 0

ulidRejectsOutOfRangeTimestamps :: Bool
ulidRejectsOutOfRangeTimestamps =
    isLeft (newULIDFromParts beforeEpoch validEntropy)
        && isLeft (newULIDFromParts afterMaximumTimestamp validEntropy)
  where
    beforeEpoch = posixSecondsToUTCTime (-0.001)
    afterMaximumTimestamp =
        posixSecondsToUTCTime
            (fromInteger (2 ^ (48 :: Int)) / 1000)
    validEntropy = ByteString.replicate 10 0

baseTime :: UTCTime
baseTime = UTCTime (fromGregorian 2026 9 12) 0

maxULIDInteger :: Integer
maxULIDInteger = (2 ^ (128 :: Int)) - 1
