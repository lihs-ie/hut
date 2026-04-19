"use server";

import { cache } from "react";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Article, UnvalidatedCriteria } from "@shared/domains/articles";
import { UnvalidatedCriteria as UnvalidatedMemoCriteria } from "@shared/domains/memo";
import { PublishStatus } from "@shared/domains/common";
import { Memo } from "@shared/domains/memo";
import { ArticleWorkflowProvider } from "@/providers/workflows/article";
import { MemoWorkflowProvider } from "@/providers/workflows/memo";

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
