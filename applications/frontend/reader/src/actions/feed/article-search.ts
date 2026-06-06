"use server";

import { cache } from "react";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Article, UnvalidatedCriteria } from "@shared/domains/articles";
import { UnvalidatedCriteria as UnvalidatedMemoCriteria } from "@shared/domains/memo";
import { PublishStatus } from "@shared/domains/common";
import { Memo } from "@shared/domains/memo";
import { Series } from "@shared/domains/series";
import { ArticleWorkflowProvider } from "@/providers/workflows/article";
import { MemoWorkflowProvider } from "@/providers/workflows/memo";
import { ReaderSeriesWorkflowProvider } from "@/providers/workflows/series";

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

export const searchMemos = cache(
  async (unvalidated: UnvalidatedMemoCriteria): Promise<Memo[]> => {
    return await unwrapForNextJs(
      MemoWorkflowProvider.search({
        now: new Date(),
        payload: {
          ...unvalidated,
          status: PublishStatus.PUBLISHED,
        },
      }),
    );
  },
);

export const searchSeries = cache(async (): Promise<Series[]> => {
  return await unwrapForNextJs(
    ReaderSeriesWorkflowProvider.search({
      slug: null,
      tags: null,
      status: "published",
      freeWord: null,
    }),
  );
});
