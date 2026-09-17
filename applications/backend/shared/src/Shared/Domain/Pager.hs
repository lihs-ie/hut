module Shared.Domain.Pager (
    Pager,
    total,
    items,
    current,
    newPager,
    offset,
    firstPage,
    lastPage,
) where

import Shared.Domain.Error (DomainError, createInvariantViolation)

type Total = Int
type Items = Int
type Current = Int

data Pager = Pager
    { total :: Total
    , items :: Items
    , current :: Current
    }
    deriving stock (Show, Eq)

newPager :: Total -> Items -> Current -> Either DomainError Pager
newPager total items current =
    if total < 0 || items < 0 || current < 0
        then
            Left $
                createInvariantViolation
                    "Pager"
                    "total, items and current must be positive or zero"
        else Right $ Pager total items current

offset :: Pager -> Int
offset pager = (current pager - 1) * items pager

firstPage :: Pager -> Int
firstPage pager = if total pager == 0 then 0 else 1

lastPage :: Pager -> Int
lastPage pager
    | total pager == 0 = 0
    | items pager <= 0 = 0
    | otherwise =
        ceiling (fromIntegral (total pager) / fromIntegral (items pager) :: Double)
