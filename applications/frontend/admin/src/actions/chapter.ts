"use server";

import { revalidateTag } from "next/cache";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Chapter, UnvalidatedChapter, ChapterIdentifier } from "@shared/domains/series/chapter";
import { addChapter } from "@shared/domains/series";
import { AdminChapterWorkflowProvider } from "@/providers/workflows/chapter";
import { AdminSeriesWorkflowProvider } from "@/providers/workflows/series";
import { requireAdmin } from "@/aspects/auth-guard";

export async function persist(
  unvalidated: UnvalidatedChapter,
  seriesSlug?: string,
): Promise<void> {
  await requireAdmin();
  await unwrapForNextJs(AdminChapterWorkflowProvider.persist(unvalidated));

  if (seriesSlug) {
    const series = await unwrapForNextJs(
      AdminSeriesWorkflowProvider.findBySlug({
        payload: { slug: seriesSlug },
        now: new Date(),
      }),
    );

    const chapterIdentifier = unvalidated.identifier as ChapterIdentifier;
    if (!series.chapters.includes(chapterIdentifier)) {
      const updatedSeries = addChapter(series, chapterIdentifier);
      await unwrapForNextJs(
        AdminSeriesWorkflowProvider.persist({
          identifier: updatedSeries.identifier,
          title: updatedSeries.title,
          slug: updatedSeries.slug,
          subTitle: updatedSeries.subTitle,
          description: updatedSeries.description,
          cover: updatedSeries.cover,
          tags: updatedSeries.tags,
          chapters: updatedSeries.chapters,
          status: updatedSeries.status,
          timeline: {
            createdAt: updatedSeries.timeline.createdAt,
            updatedAt: new Date(),
          },
        }),
      );
    }
  }

  revalidateTag("chapters", {});
  revalidateTag("series", {});
}

export async function findBySlug(slug: string): Promise<Chapter> {
  await requireAdmin();
  return await unwrapForNextJs(
    AdminChapterWorkflowProvider.findBySlug({
      payload: { slug },
      now: new Date(),
    }),
  );
}
