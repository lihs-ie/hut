"use server";

import { unwrapForNextJs } from "@shared/components/global/next-error";
import { UnvalidatedProfile } from "@shared/domains/user";
import { AdminWorkflowProvider } from "../providers/workflows/admin";

export async function persistProfile(
  unvalidated: UnvalidatedProfile,
): Promise<void> {
  return await unwrapForNextJs(
    AdminWorkflowProvider.persistProfile({
      now: new Date(),
      payload: unvalidated,
    }),
  );
}
