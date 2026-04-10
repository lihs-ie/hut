"use server";

import { revalidateTag } from "next/cache";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Series, UnvalidatedCriteria, UnvalidatedSeries } from "@shared/domains/series";
import { AdminSeriesWorkflowProvider } from "@/providers/workflows/series";
import { EventBrokerProvider } from "@/providers/domain/event";
import { requireAdmin } from "@/aspects/auth-guard";
import { notifyReaderRevalidation } from "@/aspects/revalidation";
import { REVALIDATION_TAGS } from "@shared/config/revalidation";

export async function findBySlug(slug: string): Promise<Series> {
  await requireAdmin();
  return await unwrapForNextJs(
    AdminSeriesWorkflowProvider.findBySlug({
      payload: { slug },
      now: new Date(),
    }),
  );
}

export async function persist(unvalidated: UnvalidatedSeries): Promise<void> {
  await requireAdmin();
  await unwrapForNextJs(
    AdminSeriesWorkflowProvider.persist(unvalidated)
      .andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag("series", {});
  notifyReaderRevalidation([REVALIDATION_TAGS.SERIES]);
}

export async function terminate(identifier: string): Promise<void> {
  await requireAdmin();
  await unwrapForNextJs(
    AdminSeriesWorkflowProvider.terminateWithChapters(identifier)
      .andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag("series", {});
  notifyReaderRevalidation([REVALIDATION_TAGS.SERIES]);
}

export async function search(unvalidated: UnvalidatedCriteria): Promise<Series[]> {
  await requireAdmin();
  return await unwrapForNextJs(
    AdminSeriesWorkflowProvider.search(unvalidated),
  );
}
