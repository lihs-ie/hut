"use server";

import { cache } from "react";
import { generateToc } from "@shared/components/global/mdx";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Article, UnvalidatedCriteria } from "@shared/domains/articles";
import { PublishStatus } from "@shared/domains/common";
import { ArticleWorkflowProvider } from "@shared/providers/workflows/article";

export const find = cache(async (identifier: string): Promise<Article> => {
  return await unwrapForNextJs(
    ArticleWorkflowProvider.find({ payload: { identifier }, now: new Date() }),
  );
});

export const findBySlug = cache(async (slug: string): Promise<Article> => {
  return await unwrapForNextJs(
    ArticleWorkflowProvider.findBySlug({ payload: { slug }, now: new Date() }),
  );
});

export const search = cache(
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

export const createTableOfContents = cache(async (slug: string) => {
  const article = await findBySlug(slug);

  return generateToc(article.content);
});
