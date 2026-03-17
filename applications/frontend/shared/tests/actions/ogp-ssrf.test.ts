/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";

const mockFetch = vi.fn();
global.fetch = mockFetch;

vi.mock("next/cache", () => ({
  unstable_cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

describe("OGP fetchOGP - SSRF防止", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockFetch.mockReset();
  });

  describe("プライベートIPレンジのブロック", () => {
    const privateUrls = [
      "http://10.0.0.1/secret",
      "http://10.255.255.255/secret",
      "http://172.16.0.1/secret",
      "http://172.31.255.255/secret",
      "http://192.168.0.1/secret",
      "http://192.168.255.255/secret",
      "http://127.0.0.1/secret",
      "http://localhost/secret",
      "http://169.254.0.1/secret",
      "http://0.0.0.0/secret",
    ];

    privateUrls.forEach((url) => {
      it(`プライベートIP ${url} へのアクセスをブロックする`, async () => {
        const { fetchOGP } = await import("@shared/actions/ogp");
        const result = await fetchOGP(url);

        expect(result).toEqual({ url });
        expect(mockFetch).not.toHaveBeenCalled();
      });
    });
  });

  describe("正当な外部URLは通過する", () => {
    it("https://example.com は通過する", async () => {
      mockFetch.mockResolvedValueOnce({
        ok: true,
        text: () =>
          Promise.resolve(
            "<html><head><title>Example</title></head><body></body></html>",
          ),
      });

      const { fetchOGP } = await import("@shared/actions/ogp");
      await fetchOGP("https://example.com");

      expect(mockFetch).toHaveBeenCalled();
    });

    it("https://github.com は通過する", async () => {
      mockFetch.mockResolvedValueOnce({
        ok: true,
        text: () =>
          Promise.resolve(
            "<html><head><title>GitHub</title></head><body></body></html>",
          ),
      });

      const { fetchOGP } = await import("@shared/actions/ogp");
      await fetchOGP("https://github.com");

      expect(mockFetch).toHaveBeenCalled();
    });
  });
});
