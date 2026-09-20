module Main (main) where

import Domain.Article.CommonSpec qualified as Common
import Domain.Article.CriteriaSpec qualified as Criteria
import Domain.Article.EventSpec qualified as Event
import Domain.Article.DraftSpec qualified as Draft
import Domain.Article.LifecycleSpec qualified as Lifecycle

main :: IO ()
main = do
    Common.run
    Criteria.run
    Event.run
    Draft.run
    Lifecycle.run
    putStrLn "Article domain tests passed"
