"use server";

import { ok } from "@shared/aspects/result";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Profile, UnvalidatedProfile } from "@shared/domains/user";
import { AdminWorkflowProvider } from "../providers/workflows/admin";

export async function getProfile(): Promise<Profile> {
  return await unwrapForNextJs(
    AdminWorkflowProvider.find({ now: new Date(), payload: null }).andThen(
      (admin) => ok(admin.profile),
    ),
  );
}

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
