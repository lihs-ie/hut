import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { vi } from "vitest";
import { createBaseNextConfig } from "../../next.config.shared";

describe("next.config.shared - セキュリティヘッダー", () => {
  const originalEnv = process.env;

  beforeEach(() => {
    process.env = { ...originalEnv };
    delete process.env.IMAGE_REMOTE_PATTERNS;
  });

  afterEach(() => {
    vi.unstubAllEnvs();
    process.env = originalEnv;
  });

  describe("セキュリティヘッダー", () => {
    it("X-Content-Type-Options: nosniff ヘッダーが設定される", () => {
      const config = createBaseNextConfig();

      const headers = config.headers;
      expect(headers).toBeDefined();
    });

    it("セキュリティヘッダーが全ルートに適用される", async () => {
      const config = createBaseNextConfig();

      expect(config.headers).toBeDefined();

      const headersResult = await config.headers?.();
      expect(headersResult).toBeDefined();
      expect(headersResult!.length).toBeGreaterThan(0);

      const allRoutesHeader = headersResult!.find(
        (h) => h.source === "/(.*)",
      );
      expect(allRoutesHeader).toBeDefined();

      const headerNames = allRoutesHeader!.headers.map((h) => h.key);
      expect(headerNames).toContain("X-Content-Type-Options");
      expect(headerNames).toContain("X-Frame-Options");
      expect(headerNames).toContain("Referrer-Policy");
      expect(headerNames).toContain("Permissions-Policy");
      expect(headerNames).toContain("Strict-Transport-Security");
    });

    it("X-Content-Type-Options が nosniff である", async () => {
      const config = createBaseNextConfig();
      const headersResult = await config.headers?.();
      const allRoutesHeader = headersResult!.find(
        (h) => h.source === "/(.*)",
      );

      const header = allRoutesHeader!.headers.find(
        (h) => h.key === "X-Content-Type-Options",
      );
      expect(header?.value).toBe("nosniff");
    });

    it("X-Frame-Options が DENY である", async () => {
      const config = createBaseNextConfig();
      const headersResult = await config.headers?.();
      const allRoutesHeader = headersResult!.find(
        (h) => h.source === "/(.*)",
      );

      const header = allRoutesHeader!.headers.find(
        (h) => h.key === "X-Frame-Options",
      );
      expect(header?.value).toBe("DENY");
    });

    it("Referrer-Policy が strict-origin-when-cross-origin である", async () => {
      const config = createBaseNextConfig();
      const headersResult = await config.headers?.();
      const allRoutesHeader = headersResult!.find(
        (h) => h.source === "/(.*)",
      );

      const header = allRoutesHeader!.headers.find(
        (h) => h.key === "Referrer-Policy",
      );
      expect(header?.value).toBe("strict-origin-when-cross-origin");
    });

    it("Permissions-Policy が適切に設定される", async () => {
      const config = createBaseNextConfig();
      const headersResult = await config.headers?.();
      const allRoutesHeader = headersResult!.find(
        (h) => h.source === "/(.*)",
      );

      const header = allRoutesHeader!.headers.find(
        (h) => h.key === "Permissions-Policy",
      );
      expect(header?.value).toBe(
        "camera=(), microphone=(), geolocation=()",
      );
    });

    it("Strict-Transport-Security が適切に設定される", async () => {
      const config = createBaseNextConfig();
      const headersResult = await config.headers?.();
      const allRoutesHeader = headersResult!.find(
        (h) => h.source === "/(.*)",
      );

      const header = allRoutesHeader!.headers.find(
        (h) => h.key === "Strict-Transport-Security",
      );
      expect(header?.value).toBe(
        "max-age=31536000; includeSubDomains; preload",
      );
    });

    it("Content-Security-Policy ヘッダーが設定される", async () => {
      const config = createBaseNextConfig();
      const headersResult = await config.headers?.();
      const allRoutesHeader = headersResult!.find(
        (h) => h.source === "/(.*)",
      );

      const header = allRoutesHeader!.headers.find(
        (h) => h.key === "Content-Security-Policy",
      );
      expect(header).toBeDefined();
      expect(header?.value).toContain("default-src 'self'");
      expect(header?.value).toContain("object-src 'none'");
      expect(header?.value).toContain("base-uri 'self'");
    });

    it("Content-Security-Policy の img-src に http: が含まれない", async () => {
      const config = createBaseNextConfig();
      const headersResult = await config.headers?.();
      const allRoutesHeader = headersResult!.find(
        (h) => h.source === "/(.*)",
      );

      const header = allRoutesHeader!.headers.find(
        (h) => h.key === "Content-Security-Policy",
      );
      expect(header?.value).not.toContain("img-src 'self' data: blob: https: http:");
    });

    it("本番環境では script-src に unsafe-eval が含まれない", async () => {
      vi.stubEnv("NODE_ENV", "production");
      const config = createBaseNextConfig();
      const headersResult = await config.headers?.();
      const allRoutesHeader = headersResult!.find(
        (h) => h.source === "/(.*)",
      );

      const header = allRoutesHeader!.headers.find(
        (h) => h.key === "Content-Security-Policy",
      );
      const scriptSrc = header?.value
        ?.split(";")
        .find((directive) => directive.trim().startsWith("script-src"));
      expect(scriptSrc).toBeDefined();
      expect(scriptSrc).not.toContain("unsafe-eval");
    });

    it("includeCSP が false の場合 CSP ヘッダーが含まれない", async () => {
      const config = createBaseNextConfig({ includeCSP: false });
      const headersResult = await config.headers?.();
      const allRoutesHeader = headersResult!.find(
        (h) => h.source === "/(.*)",
      );

      const header = allRoutesHeader!.headers.find(
        (h) => h.key === "Content-Security-Policy",
      );
      expect(header).toBeUndefined();
    });

    it("デフォルトで CSP ヘッダーが含まれる", async () => {
      const config = createBaseNextConfig();
      const headersResult = await config.headers?.();
      const allRoutesHeader = headersResult!.find(
        (h) => h.source === "/(.*)",
      );

      const header = allRoutesHeader!.headers.find(
        (h) => h.key === "Content-Security-Policy",
      );
      expect(header).toBeDefined();
    });
  });

  describe("allowedOrigins", () => {
    it("SERVER_ACTIONS_ALLOWED_ORIGINS が未設定でも serverActions が設定される", () => {
      delete process.env.SERVER_ACTIONS_ALLOWED_ORIGINS;
      const config = createBaseNextConfig();
      expect(config.experimental?.serverActions).toBeDefined();
      expect(config.experimental?.serverActions?.allowedOrigins).toEqual([]);
    });

    it("SERVER_ACTIONS_ALLOWED_ORIGINS が設定されている場合その値が使われる", () => {
      process.env.SERVER_ACTIONS_ALLOWED_ORIGINS = "https://example.com,https://admin.example.com";
      const config = createBaseNextConfig();
      expect(config.experimental?.serverActions?.allowedOrigins).toEqual([
        "https://example.com",
        "https://admin.example.com",
      ]);
    });
  });

  describe("poweredByHeader", () => {
    it("poweredByHeader が false に設定される", () => {
      const config = createBaseNextConfig();
      expect(config.poweredByHeader).toBe(false);
    });
  });
});
