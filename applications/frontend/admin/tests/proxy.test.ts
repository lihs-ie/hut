import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { NextRequest, NextResponse } from "next/server";

vi.mock("@/aspects/e2e", () => ({
  isE2EAuthAvailable: vi.fn(),
}));

vi.mock("@/actions/rate-limit", () => ({
  enforceLoginRateLimit: vi.fn(),
}));

vi.mock("@/aspects/ip-address", () => ({
  resolveIP: vi.fn().mockReturnValue("ip:127.0.0.1"),
}));

import { proxy, config } from "@/proxy";
import { isE2EAuthAvailable } from "@/aspects/e2e";

const createMockRequest = (
  path: string,
  options?: { method?: string; cookies?: Record<string, string> },
): NextRequest => {
  const request = new NextRequest(new URL(path, "http://localhost:3001"), {
    method: options?.method ?? "GET",
  });

  if (options?.cookies) {
    for (const [name, value] of Object.entries(options.cookies)) {
      request.cookies.set(name, value);
    }
  }

  return request;
};

describe("proxy", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe("config", () => {
    it("matcher が設定されている", () => {
      expect(config.matcher).toBeDefined();
      expect(config.matcher.length).toBeGreaterThan(0);
    });
  });

  describe("E2Eエンドポイント", () => {
    it("E2E認証が利用不可の場合 404 を返す", async () => {
      vi.mocked(isE2EAuthAvailable).mockReturnValue({
        available: false,
        reason: "Firebase emulator is not enabled",
      });

      const request = createMockRequest("/api/e2e/auth");
      const response = await proxy(request);

      expect(response.status).toBe(404);
      expect(isE2EAuthAvailable).toHaveBeenCalledWith({
        e2eAuthEnabled: process.env.E2E_AUTH_ENABLED,
        useFirebaseEmulator: process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR,
      });
    });

    it("E2E認証が利用可能の場合 next() を返す", async () => {
      vi.mocked(isE2EAuthAvailable).mockReturnValue({
        available: true,
      });

      const nextSpy = vi.spyOn(NextResponse, "next");

      const request = createMockRequest("/api/e2e/auth");
      await proxy(request);

      expect(nextSpy).toHaveBeenCalled();
    });
  });
});
