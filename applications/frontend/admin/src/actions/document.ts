"use server";

import { unwrapForNextJs } from "@shared/components/global/next-error";
import { UnvalidatedPrivacyPolicy } from "@shared/domains/document";
import { AdminDocumentWorkflowProvider } from "@/providers/workflows/document";
import { revalidatePath } from "next/cache";
import { requireAdmin } from "@/aspects/auth-guard";

export async function persistPrivacyPolicy(
  unvalidated: UnvalidatedPrivacyPolicy,
): Promise<void> {
  await requireAdmin();
  await unwrapForNextJs(
    AdminDocumentWorkflowProvider.PersistPrivatePolicy({
      now: new Date(),
      payload: unvalidated,
    }),
  );

  revalidatePath("/privacy");
}
