import { describe, it, expect, vi } from "vitest";

vi.mock("@/aspects/e2e", () => ({
  isE2EAuthAvailable: vi.fn(),
}));

vi.mock("@/actions/rate-limit", () => ({
  enforceLoginRateLimit: vi.fn(),
}));

vi.mock("@/aspects/ip-address", () => ({
  resolveIP: vi.fn().mockReturnValue("ip:127.0.0.1"),
}));

import { middleware, config } from "../src/middleware";

describe("middleware", () => {
  it("middleware として proxy がエクスポートされている", () => {
    expect(typeof middleware).toBe("function");
  });

  it("config がエクスポートされている", () => {
    expect(config).toBeDefined();
    expect(config.matcher).toBeDefined();
  });
});
