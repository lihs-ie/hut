/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { NextRequest } from "next/server";

const createRequest = (
  url: string = "https://example.com/",
  headers: Record<string, string> = {},
): NextRequest => {
  const request = new NextRequest(url);
  Object.entries(headers).forEach(([key, value]) => {
    request.headers.set(key, value);
  });
  return request;
};

describe("proxy", () => {
  beforeEach(() => {
    vi.resetModules();
    vi.unstubAllEnvs();
  });

  it("proxy では Content-Security-Policy ヘッダーを付与しない", async () => {
    const { proxy } = await import("../src/proxy");
    const request = createRequest();
    const response = await proxy(request);

    expect(response.headers.get("Content-Security-Policy")).toBeNull();
  });

  it("X-Nextjs-Cache ヘッダーがレスポンスに含まれない", async () => {
    const { proxy } = await import("../src/proxy");
    const request = createRequest();
    const response = await proxy(request);

    expect(response.headers.get("X-Nextjs-Cache")).toBeNull();
  });

  it("X-Nextjs-Prerender ヘッダーが削除される", async () => {
    const { proxy } = await import("../src/proxy");
    const request = createRequest();
    const response = await proxy(request);

    expect(response.headers.get("X-Nextjs-Prerender")).toBeNull();
  });

  it("X-Nextjs-Stale-Time ヘッダーが削除される", async () => {
    const { proxy } = await import("../src/proxy");
    const request = createRequest();
    const response = await proxy(request);

    expect(response.headers.get("X-Nextjs-Stale-Time")).toBeNull();
  });

  it("Server ヘッダーが削除される", async () => {
    const { proxy } = await import("../src/proxy");
    const request = createRequest();
    const response = await proxy(request);

    expect(response.headers.get("Server")).toBeNull();
  });

  describe("rate limiting", () => {
    it("制限内のリクエストは通常のレスポンスを返す", async () => {
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_LIMIT", "10");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_WINDOW_MS", "60000");

      const { proxy } = await import("../src/proxy");
      const request = createRequest("https://example.com/some-page", {
        "x-forwarded-for": "1.2.3.4",
      });

      const response = await proxy(request);

      expect(response.status).not.toBe(429);
    });

    it("閾値を超えたリクエストは 429 を返す", async () => {
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_LIMIT", "1");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_WINDOW_MS", "60000");

      const { proxy } = await import("../src/proxy");
      const requestIp = "10.0.0.1";
      const makeRequest = () =>
        createRequest("https://example.com/some-page", {
          "x-forwarded-for": requestIp,
        });

      await proxy(makeRequest());

      const secondResponse = await proxy(makeRequest());

      expect(secondResponse.status).toBe(429);
    });

    it("429 レスポンスに Retry-After ヘッダーが含まれる", async () => {
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_LIMIT", "1");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_WINDOW_MS", "60000");

      const { proxy } = await import("../src/proxy");
      const requestIp = "10.0.0.2";
      const makeRequest = () =>
        createRequest("https://example.com/some-page", {
          "x-forwarded-for": requestIp,
        });

      await proxy(makeRequest());

      const secondResponse = await proxy(makeRequest());

      expect(secondResponse.headers.get("Retry-After")).not.toBeNull();
    });

    it("429 レスポンスに X-RateLimit-Limit ヘッダーが含まれる", async () => {
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_LIMIT", "1");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_WINDOW_MS", "60000");

      const { proxy } = await import("../src/proxy");
      const requestIp = "10.0.0.3";
      const makeRequest = () =>
        createRequest("https://example.com/some-page", {
          "x-forwarded-for": requestIp,
        });

      await proxy(makeRequest());

      const secondResponse = await proxy(makeRequest());

      expect(secondResponse.headers.get("X-RateLimit-Limit")).toBe("1");
    });

    it("429 レスポンスに X-RateLimit-Reset ヘッダーが含まれる", async () => {
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_LIMIT", "1");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_WINDOW_MS", "60000");

      const { proxy } = await import("../src/proxy");
      const requestIp = "10.0.0.4";
      const makeRequest = () =>
        createRequest("https://example.com/some-page", {
          "x-forwarded-for": requestIp,
        });

      await proxy(makeRequest());

      const secondResponse = await proxy(makeRequest());

      expect(secondResponse.headers.get("X-RateLimit-Reset")).not.toBeNull();
    });

    it("429 レスポンスに X-RateLimit-Remaining ヘッダーが含まれる", async () => {
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_LIMIT", "1");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_WINDOW_MS", "60000");

      const { proxy } = await import("../src/proxy");
      const requestIp = "10.0.0.10";
      const makeRequest = () =>
        createRequest("https://example.com/some-page", {
          "x-forwarded-for": requestIp,
        });

      await proxy(makeRequest());
      const secondResponse = await proxy(makeRequest());

      expect(secondResponse.headers.get("X-RateLimit-Remaining")).toBe("0");
    });

    it("200 OK レスポンスにも X-RateLimit-Limit ヘッダーが含まれる", async () => {
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_LIMIT", "10");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_WINDOW_MS", "60000");

      const { proxy } = await import("../src/proxy");
      const request = createRequest("https://example.com/some-page", {
        "x-forwarded-for": "10.0.0.11",
      });

      const response = await proxy(request);

      expect(response.headers.get("X-RateLimit-Limit")).toBe("10");
    });

    it("200 OK レスポンスにも X-RateLimit-Remaining ヘッダーが含まれる", async () => {
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_LIMIT", "10");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_WINDOW_MS", "60000");

      const { proxy } = await import("../src/proxy");
      const request = createRequest("https://example.com/some-page", {
        "x-forwarded-for": "10.0.0.12",
      });

      const response = await proxy(request);

      expect(response.headers.get("X-RateLimit-Remaining")).toBe("9");
    });

    it("200 OK レスポンスにも X-RateLimit-Reset ヘッダーが含まれる", async () => {
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_LIMIT", "10");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_WINDOW_MS", "60000");

      const { proxy } = await import("../src/proxy");
      const request = createRequest("https://example.com/some-page", {
        "x-forwarded-for": "10.0.0.13",
      });

      const response = await proxy(request);

      expect(response.headers.get("X-RateLimit-Reset")).not.toBeNull();
    });

    it("429 レスポンスの Content-Type は application/json", async () => {
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_LIMIT", "1");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_WINDOW_MS", "60000");

      const { proxy } = await import("../src/proxy");
      const requestIp = "10.0.0.14";
      const makeRequest = () =>
        createRequest("https://example.com/some-page", {
          "x-forwarded-for": requestIp,
        });

      await proxy(makeRequest());
      const secondResponse = await proxy(makeRequest());

      expect(secondResponse.headers.get("Content-Type")).toContain(
        "application/json",
      );
    });

    it("allowlist に含まれる IP は制限を超えても通過する", async () => {
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_LIMIT", "1");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_WINDOW_MS", "60000");
      vi.stubEnv("READER_RATE_LIMIT_ALLOWLIST", "1.2.3.4");

      const { proxy } = await import("../src/proxy");
      const makeRequest = () =>
        createRequest("https://example.com/some-page", {
          "x-forwarded-for": "1.2.3.4",
        });

      await proxy(makeRequest());
      const secondResponse = await proxy(makeRequest());

      expect(secondResponse.status).not.toBe(429);
    });

    it("/search パスは SEARCH エンドポイントの閾値を使う", async () => {
      vi.stubEnv("READER_RATE_LIMIT_SEARCH_LIMIT", "1");
      vi.stubEnv("READER_RATE_LIMIT_SEARCH_WINDOW_MS", "60000");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_LIMIT", "100");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_WINDOW_MS", "60000");

      const { proxy } = await import("../src/proxy");
      const requestIp = "10.0.0.5";
      const makeRequest = () =>
        createRequest("https://example.com/search", {
          "x-forwarded-for": requestIp,
        });

      await proxy(makeRequest());
      const secondResponse = await proxy(makeRequest());

      expect(secondResponse.status).toBe(429);
    });

    it("/feed パスは FEED エンドポイントの閾値を使う", async () => {
      vi.stubEnv("READER_RATE_LIMIT_FEED_LIMIT", "1");
      vi.stubEnv("READER_RATE_LIMIT_FEED_WINDOW_MS", "60000");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_LIMIT", "100");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_WINDOW_MS", "60000");

      const { proxy } = await import("../src/proxy");
      const requestIp = "10.0.0.6";
      const makeRequest = () =>
        createRequest("https://example.com/feed", {
          "x-forwarded-for": requestIp,
        });

      await proxy(makeRequest());
      const secondResponse = await proxy(makeRequest());

      expect(secondResponse.status).toBe(429);
    });

    it("/searching のように prefix が一致するだけのパスは default エンドポイント扱い", async () => {
      vi.stubEnv("READER_RATE_LIMIT_SEARCH_LIMIT", "1");
      vi.stubEnv("READER_RATE_LIMIT_SEARCH_WINDOW_MS", "60000");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_LIMIT", "100");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_WINDOW_MS", "60000");

      const { proxy } = await import("../src/proxy");
      const requestIp = "10.0.0.7";
      const makeRequest = () =>
        createRequest("https://example.com/searching", {
          "x-forwarded-for": requestIp,
        });

      await proxy(makeRequest());
      const secondResponse = await proxy(makeRequest());

      expect(secondResponse.status).not.toBe(429);
    });

    it("/search/query のように子パスも SEARCH エンドポイント扱い", async () => {
      vi.stubEnv("READER_RATE_LIMIT_SEARCH_LIMIT", "1");
      vi.stubEnv("READER_RATE_LIMIT_SEARCH_WINDOW_MS", "60000");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_LIMIT", "100");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_WINDOW_MS", "60000");

      const { proxy } = await import("../src/proxy");
      const requestIp = "10.0.0.8";
      const makeRequest = () =>
        createRequest("https://example.com/search/articles", {
          "x-forwarded-for": requestIp,
        });

      await proxy(makeRequest());
      const secondResponse = await proxy(makeRequest());

      expect(secondResponse.status).toBe(429);
    });

    it("READER_RATE_LIMIT_FAIL_OPEN=true の場合はストレージ障害時も通過する", async () => {
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_LIMIT", "10");
      vi.stubEnv("READER_RATE_LIMIT_DEFAULT_WINDOW_MS", "60000");
      vi.stubEnv("READER_RATE_LIMIT_FAIL_OPEN", "true");

      const { proxy } = await import("../src/proxy");
      const request = createRequest("https://example.com/some-page", {
        "x-forwarded-for": "1.2.3.4",
      });

      const response = await proxy(request);

      expect(response.status).not.toBe(429);
    });
  });
});
