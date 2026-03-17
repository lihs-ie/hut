"use server";

import { isAdmin } from "@/actions/auth";
import { UnauthenticatedHttpError } from "@shared/components/global/next-error";

export const requireAdmin = async (): Promise<void> => {
  const authenticated = await isAdmin();

  if (!authenticated) {
    throw new UnauthenticatedHttpError("認証が必要です");
  }
};
