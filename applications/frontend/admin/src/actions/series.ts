"use server";

import { revalidateTag } from "next/cache";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { UnvalidatedSeries } from "@shared/domains/series";
import { AdminSeriesWorkflowProvider } from "@/providers/workflows/series";
import { requireAdmin } from "@/aspects/auth-guard";

export async function persist(unvalidated: UnvalidatedSeries): Promise<void> {
  await requireAdmin();
  await unwrapForNextJs(AdminSeriesWorkflowProvider.persist(unvalidated));

  revalidateTag("series", {});
}

export async function terminate(identifier: string): Promise<void> {
  await requireAdmin();
  await unwrapForNextJs(AdminSeriesWorkflowProvider.terminate(identifier));

  revalidateTag("series", {});
}
