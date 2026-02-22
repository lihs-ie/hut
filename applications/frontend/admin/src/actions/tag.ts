"use server";

import { UnvalidatedTag } from "@shared/domains/attributes/tag";
import { AdminTagWorkflowProvider } from "@/providers/workflows/tag";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { revalidateTag } from "next/cache";

export async function persist(unvalidated: UnvalidatedTag): Promise<void> {
  await unwrapForNextJs(
    AdminTagWorkflowProvider.persist({ payload: unvalidated, now: new Date() }),
  );

  revalidateTag(`tags`, {});
}

export async function terminate(identifier: string): Promise<void> {
  await unwrapForNextJs(
    AdminTagWorkflowProvider.terminate({ payload: identifier, now: new Date() }),
  );

  revalidateTag(`tags`, {});
}
