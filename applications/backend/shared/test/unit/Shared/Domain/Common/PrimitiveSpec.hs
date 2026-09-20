module Shared.Domain.Common.PrimitiveSpec (run) where

import Shared.Domain.Common.Primitive
import Shared.Domain.Error (createInvariantViolation)

run :: IO Bool
run =
    pure $
        and
            [ all rejects [0, -1, negate (10 ^ (100 :: Int))]
            , all roundTrips [1, 2, toInteger (maxBound :: Int), 10 ^ (100 :: Int)]
            , newPositiveInteger 1 == Right one
            , positiveIntegerValue (nextPositiveInteger one) == 2
            , one < nextPositiveInteger one
            , show one == "1"
            ]
  where
    rejects value =
        newPositiveInteger value
            == Left (createInvariantViolation "PositiveInteger" "value must be greater than zero")
    roundTrips value = case newPositiveInteger value of
        Left _ -> False
        Right positive ->
            positiveIntegerValue positive == value
                && positiveIntegerValue (nextPositiveInteger positive) == value + 1
