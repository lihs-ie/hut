"use server";

import { cache } from "react";
import { ok } from "@shared/aspects/result";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Profile } from "@shared/domains/user";
import { AdminWorkflowProvider } from "@shared/providers/workflows/admin";

export const getProfile = cache(async (): Promise<Profile> => {
  return await unwrapForNextJs(
    AdminWorkflowProvider.find({ now: new Date(), payload: null }).andThen(
      (admin) => ok(admin.profile),
    ),
  );
});
