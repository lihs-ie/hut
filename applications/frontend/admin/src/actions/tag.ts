"use server";

import { UnvalidatedTag } from "@shared/domains/attributes/tag";
import { TagWorkflowProvider } from "@shared/providers/workflows/tag";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { revalidateTag } from "next/cache";

export async function persist(unvalidated: UnvalidatedTag): Promise<void> {
  await unwrapForNextJs(
    TagWorkflowProvider.persist({ payload: unvalidated, now: new Date() }),
  );

  revalidateTag(`tags`, {});
}

export async function terminate(identifier: string): Promise<void> {
  await unwrapForNextJs(
    TagWorkflowProvider.terminate({ payload: identifier, now: new Date() }),
  );

  revalidateTag(`tags`, {});
}
