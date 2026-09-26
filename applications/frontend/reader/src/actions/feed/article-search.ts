"use server";

import { cache } from "react";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Article, UnvalidatedCriteria } from "@shared/domains/articles";
import { PublishStatus } from "@shared/domains/common";
import { ArticleWorkflowProvider } from "@/providers/workflows/article";
import { ReaderArticleRepositoryProvider } from "@/providers/infrastructure/articles";
import { PublishedArticlePage } from "@/infrastructures/article-worker";

/** Reads one published page for Reader screens without loading the full catalog. */
export const browseArticlesPage = cache(
  async (page: number, size: number): Promise<PublishedArticlePage> =>
    unwrapForNextJs(ReaderArticleRepositoryProvider.browse(page, size)),
);

/** Reads only the articles shown on the Reader home page. */
export const latestArticles = cache(async (): Promise<Article[]> =>
  (await browseArticlesPage(1, 6)).articles,
);

export const searchArticles = cache(
  async (unvalidated: UnvalidatedCriteria): Promise<Article[]> => {
    return await unwrapForNextJs(
      ArticleWorkflowProvider.search({
        payload: {
          ...unvalidated,
          status: PublishStatus.PUBLISHED,
        },
        now: new Date(),
      }),
    );
  },
);
