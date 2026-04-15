/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { NextRequest } from "next/server";

const createRequest = (
  url: string = "https://example.com/",
  init?: RequestInit,
) => {
  return new NextRequest(url, init);
};

const rememberEnvironmentVariable = (key: string): string | undefined => {
  return process.env[key];
};

const restoreEnvironmentVariable = (
  key: string,
  value: string | undefined,
): void => {
  if (value === undefined) {
    delete process.env[key];
  } else {
    process.env[key] = value;
  }
};

describe("proxy", () => {
  beforeEach(() => {
    vi.resetModules();
  });

  it("proxy では Content-Security-Policy ヘッダーを付与しない", async () => {
    const { proxy } = await import("../src/proxy");
    const request = createRequest();
    const response = proxy(request);

    expect(response.headers.get("Content-Security-Policy")).toBeNull();
  });

  it("X-Nextjs-Cache ヘッダーがレスポンスに含まれない", async () => {
    const { proxy } = await import("../src/proxy");
    const request = createRequest();
    const response = proxy(request);

    expect(response.headers.get("X-Nextjs-Cache")).toBeNull();
  });

  it("X-Nextjs-Prerender ヘッダーが削除される", async () => {
    const { proxy } = await import("../src/proxy");
    const request = createRequest();
    const response = proxy(request);

    expect(response.headers.get("X-Nextjs-Prerender")).toBeNull();
  });

  it("X-Nextjs-Stale-Time ヘッダーが削除される", async () => {
    const { proxy } = await import("../src/proxy");
    const request = createRequest();
    const response = proxy(request);

    expect(response.headers.get("X-Nextjs-Stale-Time")).toBeNull();
  });

  it("Server ヘッダーが削除される", async () => {
    const { proxy } = await import("../src/proxy");
    const request = createRequest();
    const response = proxy(request);

    expect(response.headers.get("Server")).toBeNull();
  });

  describe("Cloudflare access guard (D-01)", () => {
    let originalNodeEnv: string | undefined;
    let originalDisableFlag: string | undefined;
    let originalEmulatorFlag: string | undefined;

    beforeEach(() => {
      originalNodeEnv = rememberEnvironmentVariable("NODE_ENV");
      originalDisableFlag = rememberEnvironmentVariable(
        "READER_DISABLE_CLOUDFLARE_GUARD",
      );
      originalEmulatorFlag = rememberEnvironmentVariable(
        "NEXT_PUBLIC_USE_FIREBASE_EMULATOR",
      );
    });

    afterEach(() => {
      restoreEnvironmentVariable("NODE_ENV", originalNodeEnv);
      restoreEnvironmentVariable(
        "READER_DISABLE_CLOUDFLARE_GUARD",
        originalDisableFlag,
      );
      restoreEnvironmentVariable(
        "NEXT_PUBLIC_USE_FIREBASE_EMULATOR",
        originalEmulatorFlag,
      );
    });

    it("production 環境で CF-Connecting-IP が無い場合は 403 を返す", async () => {
      process.env.NODE_ENV = "production";
      delete process.env.READER_DISABLE_CLOUDFLARE_GUARD;

      const { proxy } = await import("../src/proxy");
      const request = createRequest("https://hut.lihs.dev/");
      const response = proxy(request);

      expect(response.status).toBe(403);
    });

    it("production 環境で CF-Connecting-IP ヘッダが存在する場合は通過する", async () => {
      process.env.NODE_ENV = "production";
      delete process.env.READER_DISABLE_CLOUDFLARE_GUARD;

      const { proxy } = await import("../src/proxy");
      const request = createRequest("https://hut.lihs.dev/", {
        headers: { "CF-Connecting-IP": "203.0.113.10" },
      });
      const response = proxy(request);

      expect(response.status).toBe(200);
    });

    it("development 環境では CF-Connecting-IP の有無に関わらず通過する", async () => {
      process.env.NODE_ENV = "development";
      delete process.env.READER_DISABLE_CLOUDFLARE_GUARD;

      const { proxy } = await import("../src/proxy");
      const request = createRequest("http://localhost:3000/");
      const response = proxy(request);

      expect(response.status).toBe(200);
    });

    it("READER_DISABLE_CLOUDFLARE_GUARD=true のときは production でも通過する", async () => {
      process.env.NODE_ENV = "production";
      process.env.READER_DISABLE_CLOUDFLARE_GUARD = "true";

      const { proxy } = await import("../src/proxy");
      const request = createRequest("https://hut.lihs.dev/");
      const response = proxy(request);

      expect(response.status).toBe(200);
    });

    it("NEXT_PUBLIC_USE_FIREBASE_EMULATOR=true のときは production でも通過する", async () => {
      process.env.NODE_ENV = "production";
      delete process.env.READER_DISABLE_CLOUDFLARE_GUARD;
      process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR = "true";

      const { proxy } = await import("../src/proxy");
      const request = createRequest("https://hut.lihs.dev/");
      const response = proxy(request);

      expect(response.status).toBe(200);
    });
  });
});
