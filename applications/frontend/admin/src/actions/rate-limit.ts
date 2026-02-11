"use server";

import { enforceRateLimit } from "@/aspects/rate-limit";
import { FirebaseAdminProvider } from "@/providers/auth/admin";
import { isResourceExhaustedError } from "@shared/aspects/error";

export type RateLimitResult =
  | { allowed: true }
  | { allowed: false; message: string };

const loginRateLimitWindowMs = 10 * 60 * 1000;
const loginRateLimitMaxAttempts = 5;

const normalizeRateLimitKey = (key: string): string => {
  const trimmed = key.trim();

  if (trimmed.length === 0) {
    return "unknown";
  }

  return trimmed;
};

export const enforceLoginRateLimit = async (
  key: string,
): Promise<RateLimitResult> => {
  const normalizedKey = normalizeRateLimitKey(key);

  return await enforceRateLimit(FirebaseAdminProvider.firestore.instance, {
    key: `admin_login:${normalizedKey}`,
    limit: loginRateLimitMaxAttempts,
    windowMs: loginRateLimitWindowMs,
  }).match<RateLimitResult>({
    ok: (): RateLimitResult => ({ allowed: true }),
    err: (error): RateLimitResult => {
      if (isResourceExhaustedError(error)) {
        return { allowed: false, message: error.message };
      }

      return { allowed: true };
    },
  });
};
