"use server";

import { unwrapForNextJs } from "@shared/components/global/next-error";
import { PrivacyPolicy, UnvalidatedPrivacyPolicy } from "@shared/domains/document";
import { AdminDocumentWorkflowProvider } from "@/providers/workflows/document";
import { revalidatePath } from "next/cache";
import { requireAdmin } from "@/aspects/auth-guard";

export async function getPrivacyPolicy(): Promise<PrivacyPolicy> {
  await requireAdmin();
  return await unwrapForNextJs(
    AdminDocumentWorkflowProvider.GetPrivacyPolicy({
      payload: null,
      now: new Date(),
    }),
  );
}

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
