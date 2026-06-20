import type { AsyncResult } from "../result";
import type { ServiceUnavailableError, UnexpectedError } from "../error";
import type { RateLimitWindow } from "./types";

export interface RateLimitStorage {
  increment(
    key: string,
    windowMs: number,
  ): AsyncResult<RateLimitWindow, ServiceUnavailableError | UnexpectedError>;
}
