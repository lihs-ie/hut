module Shared.Domain.Common.Primitive (
    PositiveInteger,
    newPositiveInteger,
    positiveIntegerValue,
    one,
    nextPositiveInteger,
) where

import Numeric.Natural (Natural)
import Shared.Domain.Error (DomainError, createInvariantViolation)

-- Store n - 1 so every internal value represents a strictly positive integer.
newtype PositiveInteger = PositiveInteger Natural
    deriving stock (Eq, Ord)

instance Show PositiveInteger where
    show = show . positiveIntegerValue

newPositiveInteger :: Integer -> Either DomainError PositiveInteger
newPositiveInteger value
    | value > 0 = Right (PositiveInteger (fromInteger (value - 1)))
    | otherwise = Left (createInvariantViolation "PositiveInteger" "value must be greater than zero")

positiveIntegerValue :: PositiveInteger -> Integer
positiveIntegerValue (PositiveInteger predecessor) = toInteger predecessor + 1

one :: PositiveInteger
one = PositiveInteger 0

nextPositiveInteger :: PositiveInteger -> PositiveInteger
nextPositiveInteger (PositiveInteger predecessor) = PositiveInteger (predecessor + 1)
