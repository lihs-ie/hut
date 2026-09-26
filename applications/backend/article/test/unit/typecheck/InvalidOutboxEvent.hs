module InvalidOutboxEvent where

import Domain.Article.Event (ArticleDiscarded, ArticlePublished)
import Shared.Domain.Event (Events)
import Shared.UseCase.Command (Command)
import Shared.UseCase.Outbox (Append)

invalid :: Append '[ArticlePublished] IO -> Command () -> Events '[ArticleDiscarded] -> IO ()
invalid append = append
