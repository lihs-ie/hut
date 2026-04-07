"use server";

import { unwrapForNextJs } from "@shared/components/global/next-error";
import { UnvalidatedProfile } from "@shared/domains/user";
import { AdminWorkflowProvider } from "../providers/workflows/admin";
import { requireAdmin } from "@/aspects/auth-guard";

export async function persistProfile(
  unvalidated: UnvalidatedProfile,
): Promise<void> {
  await requireAdmin();
  return await unwrapForNextJs(
    AdminWorkflowProvider.persistProfile({
      now: new Date(),
      payload: unvalidated,
    }),
  );
}
