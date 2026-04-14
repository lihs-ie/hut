export type RateLimitPolicy = {
  limit: number;
  windowMs: number;
};

export type RateLimitWindow = {
  count: number;
  resetAtMs: number;
};

export type RateLimitDecision = {
  allowed: boolean;
  count: number;
  limit: number;
  resetAtMs: number;
};
