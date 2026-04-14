import { ok, err } from "../result";
import { resourceExhaustedError } from "../error";
import type { AsyncResult } from "../result";
import type {
  ResourceExhaustedError,
  ServiceUnavailableError,
  UnexpectedError,
} from "../error";
import type { RateLimitStorage } from "./storage";
import type { RateLimitPolicy, RateLimitDecision } from "./types";

type EnforcePolicy = RateLimitPolicy & {
  failOpen: boolean;
  allowlist?: string[];
};

const ALLOWLISTED_DECISION: RateLimitDecision = {
  allowed: true,
  count: 0,
  limit: 0,
  resetAtMs: 0,
};

export const enforceRateLimit = (
  storage: RateLimitStorage,
  policy: EnforcePolicy,
  identifier: string,
): AsyncResult<
  RateLimitDecision,
  ResourceExhaustedError | ServiceUnavailableError | UnexpectedError
> => {
  if (policy.allowlist?.includes(identifier)) {
    return ok(ALLOWLISTED_DECISION).toAsync();
  }

  return storage
    .increment(identifier, policy.windowMs)
    .orElse((error) => {
      if (policy.failOpen) {
        return ok({
          allowed: true,
          count: 0,
          limit: policy.limit,
          resetAtMs: Date.now() + policy.windowMs,
        }).toAsync();
      }
      return err(error).toAsync();
    })
    .andThen((window) => {
      if (window.count > policy.limit) {
        return err(
          resourceExhaustedError("Rate limit exceeded"),
        ).toAsync();
      }

      return ok({
        allowed: true,
        count: window.count,
        limit: policy.limit,
        resetAtMs: window.resetAtMs,
      }).toAsync();
    });
};
