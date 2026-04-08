/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { NextRequest } from "next/server";

vi.mock("crypto", () => ({
  randomUUID: vi.fn(() => "test-uuid-1234-5678-abcd-ef0123456789"),
}));

const createRequest = (url: string = "https://example.com/") => {
  return new NextRequest(url);
};

describe("proxy", () => {
  beforeEach(() => {
    vi.resetModules();
  });

  it("nonce が生成され CSP ヘッダーに含まれる", async () => {
    const { proxy } = await import("../src/proxy");
    const request = createRequest();
    const response = proxy(request);

    const csp = response.headers.get("Content-Security-Policy");
    expect(csp).toBeDefined();
    expect(csp).toContain("nonce-");
  });

  it("CSP ヘッダーに nonce が含まれる", async () => {
    const { proxy } = await import("../src/proxy");
    const request = createRequest();
    const response = proxy(request);

    const csp = response.headers.get("Content-Security-Policy");
    expect(csp).toContain("nonce-");
  });

  it("CSP の script-src に unsafe-inline が含まれない", async () => {
    vi.stubEnv("NODE_ENV", "production");
    const { proxy } = await import("../src/proxy");
    const request = createRequest();
    const response = proxy(request);

    const csp = response.headers.get("Content-Security-Policy");
    const scriptSrc = csp
      ?.split(";")
      .find((directive) => directive.trim().startsWith("script-src"));
    expect(scriptSrc).toBeDefined();
    expect(scriptSrc).not.toContain("unsafe-inline");
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

  it("CSP ヘッダーに default-src 'self' が含まれる", async () => {
    const { proxy } = await import("../src/proxy");
    const request = createRequest();
    const response = proxy(request);

    const csp = response.headers.get("Content-Security-Policy");
    expect(csp).toContain("default-src 'self'");
  });
});
