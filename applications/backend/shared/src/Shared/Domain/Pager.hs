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

data Pager = Pager Total Items Current
    deriving stock (Show, Eq)

total :: Pager -> Total
total (Pager value _ _) = value

items :: Pager -> Items
items (Pager _ value _) = value

current :: Pager -> Current
current (Pager _ _ value) = value

newPager :: Total -> Items -> Current -> Either DomainError Pager
newPager count size page =
    if count < 0 || size <= 0 || page <= 0
        then
            Left $
                createInvariantViolation
                    "Pager"
                    "total must be nonnegative; items and current must be positive"
        else
            if (toInteger page - 1) * toInteger size > toInteger (maxBound :: Int)
                then Left (createInvariantViolation "Pager" "offset exceeds supported range")
                else Right $ Pager count size page

offset :: Pager -> Int
offset pager = (current pager - 1) * items pager

firstPage :: Pager -> Int
firstPage pager = if total pager == 0 then 0 else 1

lastPage :: Pager -> Int
lastPage pager
    | total pager == 0 = 0
    | otherwise = 1 + (total pager - 1) `div` items pager
