/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";

const mockFetch = vi.fn();
global.fetch = mockFetch;

describe("notifyReaderRevalidation", () => {
  const originalEnv = process.env;

  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    process.env = {
      ...originalEnv,
      READER_ENDPOINT: "http://reader.test",
      REVALIDATION_SECRET: "test-secret",
    };
    mockFetch.mockResolvedValue(new Response(null, { status: 200 }));
  });

  afterEach(() => {
    process.env = originalEnv;
  });

  it("正しい URL に POST リクエストを送信する", async () => {
    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    notifyReaderRevalidation(["articles"]);

    await new Promise((resolve) => setTimeout(resolve, 0));

    expect(mockFetch).toHaveBeenCalledWith(
      "http://reader.test/api/revalidate",
      expect.objectContaining({
        method: "POST",
      }),
    );
  });

  it("正しいヘッダーを設定する", async () => {
    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    notifyReaderRevalidation(["articles"]);

    await new Promise((resolve) => setTimeout(resolve, 0));

    expect(mockFetch).toHaveBeenCalledWith(
      expect.any(String),
      expect.objectContaining({
        headers: expect.objectContaining({
          "content-type": "application/json",
          "x-revalidation-secret": "test-secret",
        }),
      }),
    );
  });

  it("正しい body を送信する", async () => {
    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    notifyReaderRevalidation(["articles", "series"]);

    await new Promise((resolve) => setTimeout(resolve, 0));

    expect(mockFetch).toHaveBeenCalledWith(
      expect.any(String),
      expect.objectContaining({
        body: JSON.stringify({ tags: ["articles", "series"] }),
      }),
    );
  });

  it("fetch が失敗しても throw しない", async () => {
    mockFetch.mockRejectedValue(new Error("Network error"));

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    await expect(
      (async () => {
        notifyReaderRevalidation(["articles"]);
        await new Promise((resolve) => setTimeout(resolve, 10));
      })(),
    ).resolves.not.toThrow();
  });

  it("fetch が失敗した場合 console.error を呼び出す", async () => {
    const consoleErrorSpy = vi.spyOn(console, "error").mockImplementation(() => {});
    mockFetch.mockRejectedValue(new Error("Network error"));

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    notifyReaderRevalidation(["articles"]);
    await new Promise((resolve) => setTimeout(resolve, 10));

    expect(consoleErrorSpy).toHaveBeenCalled();
    consoleErrorSpy.mockRestore();
  });
});
