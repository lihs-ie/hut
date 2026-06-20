import { ok, err } from "../result";
import type { AsyncResult } from "../result";
import type {
  ServiceUnavailableError,
  UnexpectedError,
} from "../error";
import type { RateLimitStorage } from "./storage";
import type {
  RateLimitPolicy,
  RateLimitDecision,
  RateLimitWindow,
} from "./types";

type EnforcePolicy = RateLimitPolicy & {
  failOpen: boolean;
  allowlist?: ReadonlyArray<string>;
};

const createAllowlistedDecision = (policy: EnforcePolicy): RateLimitDecision =>
  Object.freeze({
    allowed: true,
    count: 0,
    limit: policy.limit,
    remaining: policy.limit,
    resetAtMs: Date.now() + policy.windowMs,
  });

const createFailOpenDecision = (policy: EnforcePolicy): RateLimitDecision =>
  Object.freeze({
    allowed: true,
    count: 0,
    limit: policy.limit,
    remaining: policy.limit,
    resetAtMs: Date.now() + policy.windowMs,
  });

const toDecision = (
  policy: EnforcePolicy,
  window: RateLimitWindow,
): RateLimitDecision => ({
  allowed: window.count <= policy.limit,
  count: window.count,
  limit: policy.limit,
  remaining: Math.max(0, policy.limit - window.count),
  resetAtMs: window.resetAtMs,
});

export const enforceRateLimit = (
  storage: RateLimitStorage,
  policy: EnforcePolicy,
  identifier: string,
): AsyncResult<
  RateLimitDecision,
  ServiceUnavailableError | UnexpectedError
> => {
  if (policy.allowlist?.includes(identifier)) {
    return ok<RateLimitDecision, ServiceUnavailableError | UnexpectedError>(
      createAllowlistedDecision(policy),
    ).toAsync();
  }

  return storage
    .increment(identifier, policy.windowMs)
    .map((window) => toDecision(policy, window))
    .orElse((error) => {
      if (policy.failOpen) {
        return ok<RateLimitDecision, ServiceUnavailableError | UnexpectedError>(
          createFailOpenDecision(policy),
        ).toAsync();
      }

      return err<RateLimitDecision, ServiceUnavailableError | UnexpectedError>(
        error,
      ).toAsync();
    });
};
