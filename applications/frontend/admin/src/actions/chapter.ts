"use server";

import { revalidateTag } from "next/cache";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import {
  Chapter,
  ChapterIdentifier,
  UnvalidatedChapter,
  createChapterPersistedEvent,
  createChapterTerminatedEvent,
} from "@shared/domains/series/chapter";
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
      AdminChapterWorkflowProvider.persist(unvalidated),
    );
  }

  await unwrapForNextJs(
    EventBrokerProvider.pubSub.publish(
      createChapterPersistedEvent(unvalidated.identifier as ChapterIdentifier),
    ),
  );

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

  await unwrapForNextJs(
    EventBrokerProvider.pubSub.publish(
      createChapterTerminatedEvent(chapterIdentifier as ChapterIdentifier),
    ),
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
