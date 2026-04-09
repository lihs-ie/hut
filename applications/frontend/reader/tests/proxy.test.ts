/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { NextRequest } from "next/server";

const createRequest = (url: string = "https://example.com/") => {
  return new NextRequest(url);
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
});
