/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";

const mockFetch = vi.fn();

vi.stubGlobal("fetch", mockFetch);

describe("notifyReaderRevalidation", () => {
  const originalEnv = process.env;

  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    process.env = {
      ...originalEnv,
      READER_URL: "https://reader.example.com",
      REVALIDATION_SECRET: "test-secret",
    };
  });

  afterEach(() => {
    process.env = originalEnv;
  });

  it("Reader の /api/revalidate エンドポイントに POST リクエストを送信する", async () => {
    mockFetch.mockResolvedValue({ ok: true, status: 200 });

    const { notifyReaderRevalidation } = await import("@/lib/revalidation");
    await notifyReaderRevalidation(["articles"]);

    expect(mockFetch).toHaveBeenCalledWith(
      "https://reader.example.com/api/revalidate",
      expect.objectContaining({
        method: "POST",
      }),
    );
  });

  it("x-revalidation-secret ヘッダーにシークレットを設定する", async () => {
    mockFetch.mockResolvedValue({ ok: true, status: 200 });

    const { notifyReaderRevalidation } = await import("@/lib/revalidation");
    await notifyReaderRevalidation(["articles"]);

    expect(mockFetch).toHaveBeenCalledWith(
      expect.any(String),
      expect.objectContaining({
        headers: expect.objectContaining({
          "x-revalidation-secret": "test-secret",
        }),
      }),
    );
  });

  it("リクエストボディに tags を含める", async () => {
    mockFetch.mockResolvedValue({ ok: true, status: 200 });

    const { notifyReaderRevalidation } = await import("@/lib/revalidation");
    await notifyReaderRevalidation(["articles", "series"]);

    expect(mockFetch).toHaveBeenCalledWith(
      expect.any(String),
      expect.objectContaining({
        body: JSON.stringify({ tags: ["articles", "series"] }),
      }),
    );
  });

  it("fetch が失敗してもエラーをスローしない（fire-and-forget）", async () => {
    mockFetch.mockRejectedValue(new Error("Network error"));

    const { notifyReaderRevalidation } = await import("@/lib/revalidation");

    await expect(notifyReaderRevalidation(["articles"])).resolves.not.toThrow();
  });

  it("レスポンスが 4xx/5xx でもエラーをスローしない", async () => {
    mockFetch.mockResolvedValue({ ok: false, status: 500 });

    const { notifyReaderRevalidation } = await import("@/lib/revalidation");

    await expect(notifyReaderRevalidation(["articles"])).resolves.not.toThrow();
  });

  it("READER_URL が未設定の場合はリクエストを送信しない", async () => {
    delete process.env.READER_URL;

    const { notifyReaderRevalidation } = await import("@/lib/revalidation");
    await notifyReaderRevalidation(["articles"]);

    expect(mockFetch).not.toHaveBeenCalled();
  });

  it("REVALIDATION_SECRET が未設定の場合はリクエストを送信しない", async () => {
    delete process.env.REVALIDATION_SECRET;

    const { notifyReaderRevalidation } = await import("@/lib/revalidation");
    await notifyReaderRevalidation(["articles"]);

    expect(mockFetch).not.toHaveBeenCalled();
  });

  it("空の tags 配列でもリクエストを送信する", async () => {
    mockFetch.mockResolvedValue({ ok: true, status: 200 });

    const { notifyReaderRevalidation } = await import("@/lib/revalidation");
    await notifyReaderRevalidation([]);

    expect(mockFetch).toHaveBeenCalled();
  });
});
