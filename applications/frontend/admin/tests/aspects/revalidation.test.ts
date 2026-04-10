/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";

const mockFetch = vi.fn();

vi.stubGlobal("fetch", mockFetch);

const mockConfig: {
  revalidation: { readerUrl: string | undefined; secret: string | undefined };
} = {
  revalidation: {
    readerUrl: "https://reader.example.com",
    secret: "test-secret",
  },
};

vi.mock("@/config/revalidation", () => mockConfig);

describe("notifyReaderRevalidation", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    mockConfig.revalidation = {
      readerUrl: "https://reader.example.com",
      secret: "test-secret",
    };
  });

  it("Reader の /api/revalidate エンドポイントに POST リクエストを送信する", async () => {
    mockFetch.mockResolvedValue({ ok: true, status: 200 });

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");
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

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");
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

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");
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

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    await expect(notifyReaderRevalidation(["articles"])).resolves.not.toThrow();
  });

  it("レスポンスが 4xx/5xx でもエラーをスローしない", async () => {
    mockFetch.mockResolvedValue({ ok: false, status: 500 });

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    await expect(notifyReaderRevalidation(["articles"])).resolves.not.toThrow();
  });

  it("readerUrl が未設定の場合はリクエストを送信しない", async () => {
    mockConfig.revalidation = {
      readerUrl: undefined,
      secret: "test-secret",
    };

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");
    await notifyReaderRevalidation(["articles"]);

    expect(mockFetch).not.toHaveBeenCalled();
  });

  it("secret が未設定の場合はリクエストを送信しない", async () => {
    mockConfig.revalidation = {
      readerUrl: "https://reader.example.com",
      secret: undefined,
    };

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");
    await notifyReaderRevalidation(["articles"]);

    expect(mockFetch).not.toHaveBeenCalled();
  });

  it("空の tags 配列でもリクエストを送信する", async () => {
    mockFetch.mockResolvedValue({ ok: true, status: 200 });

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");
    await notifyReaderRevalidation([]);

    expect(mockFetch).toHaveBeenCalled();
  });
});
