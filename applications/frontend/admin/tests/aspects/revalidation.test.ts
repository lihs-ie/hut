/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";

const mockFetch = vi.fn();

vi.stubGlobal("fetch", mockFetch);

const mockConfig: {
  revalidation:
    | { readerEndpoint: string; secret: string }
    | undefined;
} = {
  revalidation: {
    readerEndpoint: "https://reader.example.com",
    secret: "test-secret",
  },
};

vi.mock("@/config/revalidation", () => mockConfig);

const flushMicrotasks = () => new Promise((resolve) => setTimeout(resolve, 0));

describe("notifyReaderRevalidation", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    mockConfig.revalidation = {
      readerEndpoint: "https://reader.example.com",
      secret: "test-secret",
    };
  });

  it("Reader の /api/revalidate エンドポイントに POST リクエストを送信する", async () => {
    mockFetch.mockResolvedValue({ ok: true, status: 200 });

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");
    notifyReaderRevalidation(["articles"]);

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
    notifyReaderRevalidation(["articles"]);

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
    notifyReaderRevalidation(["articles", "series"]);

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

    expect(() => notifyReaderRevalidation(["articles"])).not.toThrow();
    await flushMicrotasks();
  });

  it("レスポンスが 4xx/5xx でもエラーをスローしない", async () => {
    mockFetch.mockResolvedValue({ ok: false, status: 500 });

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    expect(() => notifyReaderRevalidation(["articles"])).not.toThrow();
    await flushMicrotasks();
  });

  it("revalidation config が undefined の場合はリクエストを送信しない", async () => {
    mockConfig.revalidation = undefined;

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");
    notifyReaderRevalidation(["articles"]);

    expect(mockFetch).not.toHaveBeenCalled();
  });

  it("空の tags 配列でもリクエストを送信する", async () => {
    mockFetch.mockResolvedValue({ ok: true, status: 200 });

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");
    notifyReaderRevalidation([]);

    expect(mockFetch).toHaveBeenCalled();
  });
});
