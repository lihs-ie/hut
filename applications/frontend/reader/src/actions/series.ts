"use server";

import { cache } from "react";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Series } from "@shared/domains/series";
import { ReaderSeriesWorkflowProvider } from "@/providers/workflows/series";
import { findPublishedChaptersByIdentifiers } from "@/actions/chapter";

export const findBySlug = cache(async (slug: string): Promise<Series> => {
  return await unwrapForNextJs(
    ReaderSeriesWorkflowProvider.findBySlug({ payload: { slug }, now: new Date() }),
  );
});

export const searchAllSlugs = cache(async (): Promise<string[]> => {
  const seriesList = await unwrapForNextJs(
    ReaderSeriesWorkflowProvider.search({
      slug: null,
      tags: null,
      status: "published",
      freeWord: null,
    }),
  );

  return seriesList.map((series) => series.slug);
});

export const searchAllChapterParams = cache(
  async (): Promise<{ slug: string; chapter: string }[]> => {
    const seriesList = await unwrapForNextJs(
      ReaderSeriesWorkflowProvider.search({
        slug: null,
        tags: null,
        status: "published",
        freeWord: null,
      }),
    );

    const chapterResults = await Promise.all(
      seriesList.map(async (series) => {
        const chapters = await findPublishedChaptersByIdentifiers(series.chapters);
        return chapters.map((chapter) => ({
          slug: series.slug,
          chapter: chapter.slug,
        }));
      }),
    );

    return chapterResults.flat();
  },
);
