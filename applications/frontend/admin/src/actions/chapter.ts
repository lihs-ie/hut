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
import { ChapterRepositoryProvider } from "@shared/providers/infrastructure/chapter";
import { EventBrokerProvider } from "@/providers/domain/event";
import { requireAdmin } from "@/aspects/auth-guard";
import { notifyReaderRevalidation } from "@/aspects/revalidation";
import { REVALIDATION_TAGS } from "@shared/config/revalidation";

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

  revalidateTag("chapters");
  revalidateTag("series");
  notifyReaderRevalidation([REVALIDATION_TAGS.CHAPTERS, REVALIDATION_TAGS.SERIES]);
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

  revalidateTag("chapters");
  revalidateTag("series");
  notifyReaderRevalidation([REVALIDATION_TAGS.CHAPTERS, REVALIDATION_TAGS.SERIES]);
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

export const findChapterBySlug = findBySlug;

export async function findChaptersByIdentifiers(
  identifiers: ChapterIdentifier[],
): Promise<Chapter[]> {
  await requireAdmin();
  return await unwrapForNextJs(
    ChapterRepositoryProvider.firebase.ofIdentifiers(identifiers),
  );
}
