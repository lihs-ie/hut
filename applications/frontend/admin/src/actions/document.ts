"use server";

import { unwrapForNextJs } from "@shared/components/global/next-error";
import { UnvalidatedPrivacyPolicy } from "@shared/domains/document";
import { AdminDocumentWorkflowProvider } from "@/providers/workflows/document";
import { revalidatePath } from "next/cache";

export async function persistPrivacyPolicy(
  unvalidated: UnvalidatedPrivacyPolicy,
): Promise<void> {
  await unwrapForNextJs(
    AdminDocumentWorkflowProvider.PersistPrivatePolicy({
      now: new Date(),
      payload: unvalidated,
    }),
  );

  revalidatePath("/privacy");
}
