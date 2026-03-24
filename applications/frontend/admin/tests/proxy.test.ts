import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { NextRequest, NextResponse } from "next/server";
import { ok, err } from "@shared/aspects/result";

vi.mock("@/aspects/e2e", () => ({
  isE2EAuthAvailable: vi.fn(),
}));

vi.mock("@/actions/rate-limit", () => ({
  enforceLoginRateLimit: vi.fn(),
}));

vi.mock("@/aspects/ip-address", () => ({
  resolveIP: vi.fn().mockReturnValue("ip:127.0.0.1"),
}));

vi.mock("@/providers/acl/oidc/server", () => ({
  OIDCServerProvider: {
    verifySessionCookie: vi.fn(),
  },
}));

import { proxy, config } from "@/proxy";
import { isE2EAuthAvailable } from "@/aspects/e2e";
import { OIDCServerProvider } from "@/providers/acl/oidc/server";

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

  describe("セッション検証", () => {
    it("cookieがない保護ルートではログインページにリダイレクトする", async () => {
      const request = createMockRequest("/admin/articles");
      const response = await proxy(request);

      expect(response.status).toBe(307);
      expect(response.headers.get("location")).toBe(
        "http://localhost:3001/admin/login",
      );
    });

    it("有効なcookieがある保護ルートでは通過する", async () => {
      vi.mocked(OIDCServerProvider.verifySessionCookie).mockReturnValue(
        ok({
          uid: "test-uid",
          email: "test@example.com",
          displayName: null,
          photoURL: null,
        }).toAsync(),
      );

      const request = createMockRequest("/admin/articles", {
        cookies: { admin_session: "valid-session" },
      });
      const response = await proxy(request);

      expect(response.status).toBe(200);
      expect(OIDCServerProvider.verifySessionCookie).toHaveBeenCalledWith(
        "valid-session",
      );
    });

    it("不正なcookieがある保護ルートではログインページにリダイレクトしcookieを削除する", async () => {
      vi.mocked(OIDCServerProvider.verifySessionCookie).mockReturnValue(
        err({ type: "invalidCredential", message: "Invalid session" }).toAsync(),
      );

      const request = createMockRequest("/admin/articles", {
        cookies: { admin_session: "invalid-session" },
      });
      const response = await proxy(request);

      expect(response.status).toBe(307);
      expect(response.headers.get("location")).toBe(
        "http://localhost:3001/admin/login",
      );
      expect(
        response.headers.get("set-cookie"),
      ).toContain("admin_session=;");
    });

    it("ログインページに有効なcookieがある場合はトップにリダイレクトする", async () => {
      vi.mocked(OIDCServerProvider.verifySessionCookie).mockReturnValue(
        ok({
          uid: "test-uid",
          email: "test@example.com",
          displayName: null,
          photoURL: null,
        }).toAsync(),
      );

      const request = createMockRequest("/admin/login", {
        cookies: { admin_session: "valid-session" },
      });
      const response = await proxy(request);

      expect(response.status).toBe(307);
      expect(response.headers.get("location")).toBe(
        "http://localhost:3001/",
      );
    });

    it("ログインページに不正なcookieがある場合はcookieを削除してログインページを表示する", async () => {
      vi.mocked(OIDCServerProvider.verifySessionCookie).mockReturnValue(
        err({ type: "invalidCredential", message: "Invalid session" }).toAsync(),
      );

      const request = createMockRequest("/admin/login", {
        cookies: { admin_session: "invalid-session" },
      });
      const response = await proxy(request);

      expect(response.status).toBe(200);
      expect(
        response.headers.get("set-cookie"),
      ).toContain("admin_session=;");
    });
  });
});
