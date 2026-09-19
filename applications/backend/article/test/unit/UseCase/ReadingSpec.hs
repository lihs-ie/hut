module UseCase.ReadingSpec (run) where

import Control.Monad (forM_)
import Shared.Domain.Pager qualified as Pager
import TestSupport
import UseCase.Reading
import UseCase.ReadingSupport

run :: IO ()
run = do
    articles <- states
    let filters = [UnvalidatedOnly, ProofreadedOnly, ReadyOnly, PublishedOnly, PrivateOnly]
    forM_ (zip [0 :: Int ..] filters) $ \(index, status) ->
        forM_ (zip [0 :: Int ..] articles) $ \(position, article) ->
            check "exact state selection" (matchesFilter status article == (index == position))
    check "all states" (all (matchesFilter AllArticles) articles)
    defaultPage <- right (newPageRequest 1 Nothing)
    check "default ten" (pageNumber defaultPage == 1 && pageSize defaultPage == 10 && pageOffset defaultPage == 0)
    forM_ [(0, Nothing), (-1, Just 10), (1, Just 0), (1, Just (-1)), (1, Just 101), (maxBound, Just 100)] $ \(current, items) ->
        checkFailure (newPageRequest current items)
    forM_ [1, 100] $ \size -> do
        request <- right (newPageRequest 2 (Just size))
        check "size boundary and offset" (pageSize request == size && pageOffset request == size)
    second <- right (newPageRequest 2 (Just 2))
    result <- right (pageResult second 3 [()])
    check "partial last page" (Pager.total result == 3 && Pager.current result == 2 && Pager.lastPage result == 2)
    empty <- right (pageResult second 0 ([] :: [()]))
    check "empty beyond last preserves request" (Pager.current empty == 2 && Pager.total empty == 0)
    forM_ [(-1, []), (3, []), (3, [(), ()]), (0, [()])] $ \(total, values) ->
        checkFailure (pageResult second total values)
