"use server";

import { cache } from "react";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Series } from "@shared/domains/series";
import { SeriesWorkflowProvider } from "@shared/providers/workflows/series";

export const findBySlug = cache(async (slug: string): Promise<Series> => {
  return await unwrapForNextJs(
    SeriesWorkflowProvider.findBySlug({ payload: { slug }, now: new Date() }),
  );
});

export const searchAllSlugs = cache(async (): Promise<string[]> => {
  const seriesList = await unwrapForNextJs(
    SeriesWorkflowProvider.search({ slug: null, tags: null }),
  );

  return seriesList.map((series) => series.slug);
});
