"use server";

import { revalidateTag } from "next/cache";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Chapter, UnvalidatedChapter } from "@shared/domains/series/chapter";
import { AdminChapterWorkflowProvider } from "@/providers/workflows/chapter";
import { EventBrokerProvider } from "@/providers/domain/event";
import { requireAdmin } from "@/aspects/auth-guard";

export async function persist(
  unvalidated: UnvalidatedChapter,
  seriesSlug?: string,
): Promise<void> {
  await requireAdmin();

  if (seriesSlug) {
    await unwrapForNextJs(
      AdminChapterWorkflowProvider.persistWithSeries(unvalidated, seriesSlug),
    );
  } else {
    await unwrapForNextJs(
      AdminChapterWorkflowProvider.persist(unvalidated)
        .andThen(EventBrokerProvider.pubSub.publish),
    );
  }

  revalidateTag("chapters", {});
  revalidateTag("series", {});
}

export async function terminate(
  chapterIdentifier: string,
  seriesSlug: string,
): Promise<void> {
  await requireAdmin();

  await unwrapForNextJs(
    AdminChapterWorkflowProvider.terminateWithSeries(chapterIdentifier, seriesSlug),
  );

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
