module Shared.Domain.PagerSpec (run) where

import Shared.Domain.Pager

run :: IO Bool
run =
    pure $
        and
            [ all
                rejected
                [ newPager (-1) 10 1
                , newPager 0 0 1
                , newPager 0 10 0
                , newPager 0 (-1) 1
                , newPager 0 1 (-1)
                , newPager 0 100 maxBound
                ]
            , valid 0 10 1 0 0 0
            , valid 21 10 3 20 1 3
            , valid 20 10 2 10 1 2
            , valid 1 10 4 30 1 1
            , valid maxBound 1 maxBound (maxBound - 1) 1 maxBound
            , valid maxBound maxBound 2 maxBound 1 1
            ]
  where
    rejected (Left _) = True
    rejected (Right _) = False
    valid count size page start first final = case newPager count size page of
        Left _ -> False
        Right pager ->
            total pager == count
                && items pager == size
                && current pager == page
                && offset pager == start
                && firstPage pager == first
                && lastPage pager == final
