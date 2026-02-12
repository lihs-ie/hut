"use server";

import { cache } from "react";
import { generateToc } from "@shared/components/global/mdx";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Article } from "@shared/domains/articles";
import { ArticleWorkflowProvider } from "@/providers/workflows/article";

export const findBySlug = cache(async (slug: string): Promise<Article> => {
  return await unwrapForNextJs(
    ArticleWorkflowProvider.findBySlug({
      payload: { slug },
      now: new Date(),
    }),
  );
});

export const createTableOfContents = cache(async (slug: string) => {
  const article = await findBySlug(slug);

  return generateToc(article.content);
});
