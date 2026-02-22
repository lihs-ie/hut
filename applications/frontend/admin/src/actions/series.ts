"use server";

import { revalidateTag } from "next/cache";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { UnvalidatedSeries } from "@shared/domains/series";
import { AdminSeriesWorkflowProvider } from "@/providers/workflows/series";

export async function persist(unvalidated: UnvalidatedSeries): Promise<void> {
  await unwrapForNextJs(AdminSeriesWorkflowProvider.persist(unvalidated));

  revalidateTag("series", {});
}

export async function terminate(identifier: string): Promise<void> {
  await unwrapForNextJs(AdminSeriesWorkflowProvider.terminate(identifier));

  revalidateTag("series", {});
}
