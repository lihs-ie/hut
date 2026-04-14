/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";

const mockFetch = vi.fn();

vi.mock("next/server", () => ({
  after: (callback: () => Promise<void> | void) => {
    void callback();
  },
}));

describe("notifyReaderRevalidation", () => {
  const originalEnv = process.env;

  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    vi.stubGlobal("fetch", mockFetch);
    process.env = {
      ...originalEnv,
      READER_ENDPOINT: "http://reader.test",
      REVALIDATION_SECRET: "test-secret",
    };
    mockFetch.mockImplementation(async (input: RequestInfo | URL) => {
      const url = typeof input === "string" ? input : input.toString();
      if (url.startsWith("http://metadata.google.internal")) {
        return new Response(null, { status: 404 });
      }
      return new Response(null, { status: 200 });
    });
  });

  afterEach(() => {
    vi.unstubAllGlobals();
    process.env = originalEnv;
  });

  const waitForAfter = () =>
    new Promise((resolve) => setTimeout(resolve, 10));

  it("正しい URL に POST リクエストを送信する", async () => {
    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    notifyReaderRevalidation(["articles"]);
    await waitForAfter();

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
    await waitForAfter();

    expect(mockFetch).toHaveBeenCalledWith(
      "http://reader.test/api/revalidate",
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
    await waitForAfter();

    expect(mockFetch).toHaveBeenCalledWith(
      "http://reader.test/api/revalidate",
      expect.objectContaining({
        body: JSON.stringify({ tags: ["articles", "series"] }),
      }),
    );
  });

  it("AbortSignal.timeout を signal として設定する", async () => {
    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    notifyReaderRevalidation(["articles"]);
    await waitForAfter();

    expect(mockFetch).toHaveBeenCalledWith(
      "http://reader.test/api/revalidate",
      expect.objectContaining({
        signal: expect.any(AbortSignal),
      }),
    );
  });

  it("metadata server から ID トークンを取得して Authorization ヘッダを付与する", async () => {
    mockFetch.mockImplementation(async (input: RequestInfo | URL) => {
      const url = typeof input === "string" ? input : input.toString();
      if (url.startsWith("http://metadata.google.internal")) {
        return new Response("test-identity-token", { status: 200 });
      }
      return new Response(null, { status: 200 });
    });

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    notifyReaderRevalidation(["articles"]);
    await waitForAfter();

    expect(mockFetch).toHaveBeenCalledWith(
      expect.stringContaining(
        "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/identity",
      ),
      expect.objectContaining({
        headers: { "Metadata-Flavor": "Google" },
      }),
    );
    expect(mockFetch).toHaveBeenCalledWith(
      "http://reader.test/api/revalidate",
      expect.objectContaining({
        headers: expect.objectContaining({
          authorization: "Bearer test-identity-token",
        }),
      }),
    );
  });

  it("metadata server から audience に reader endpoint を指定する", async () => {
    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    notifyReaderRevalidation(["articles"]);
    await waitForAfter();

    expect(mockFetch).toHaveBeenCalledWith(
      expect.stringContaining(
        `audience=${encodeURIComponent("http://reader.test")}`,
      ),
      expect.any(Object),
    );
  });

  it("metadata server が 404 を返す場合は Authorization ヘッダを付与しない", async () => {
    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    notifyReaderRevalidation(["articles"]);
    await waitForAfter();

    const readerCall = mockFetch.mock.calls.find(
      ([url]: [string]) => url === "http://reader.test/api/revalidate",
    );
    expect(readerCall).toBeDefined();
    expect(readerCall?.[1].headers).not.toHaveProperty("authorization");
  });

  it("metadata server への fetch が失敗した場合は Authorization ヘッダを付与しない", async () => {
    mockFetch.mockImplementation(async (input: RequestInfo | URL) => {
      const url = typeof input === "string" ? input : input.toString();
      if (url.startsWith("http://metadata.google.internal")) {
        throw new Error("ECONNREFUSED");
      }
      return new Response(null, { status: 200 });
    });

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    notifyReaderRevalidation(["articles"]);
    await waitForAfter();

    const readerCall = mockFetch.mock.calls.find(
      ([url]: [string]) => url === "http://reader.test/api/revalidate",
    );
    expect(readerCall).toBeDefined();
    expect(readerCall?.[1].headers).not.toHaveProperty("authorization");
  });

  it("metadata server が空文字列を返す場合は Authorization ヘッダを付与しない", async () => {
    mockFetch.mockImplementation(async (input: RequestInfo | URL) => {
      const url = typeof input === "string" ? input : input.toString();
      if (url.startsWith("http://metadata.google.internal")) {
        return new Response("   ", { status: 200 });
      }
      return new Response(null, { status: 200 });
    });

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    notifyReaderRevalidation(["articles"]);
    await waitForAfter();

    const readerCall = mockFetch.mock.calls.find(
      ([url]: [string]) => url === "http://reader.test/api/revalidate",
    );
    expect(readerCall).toBeDefined();
    expect(readerCall?.[1].headers).not.toHaveProperty("authorization");
  });

  it("fetch が失敗しても throw しない", async () => {
    mockFetch.mockImplementation(async (input: RequestInfo | URL) => {
      const url = typeof input === "string" ? input : input.toString();
      if (url.startsWith("http://metadata.google.internal")) {
        return new Response(null, { status: 404 });
      }
      throw new Error("Network error");
    });

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    await expect(
      (async () => {
        notifyReaderRevalidation(["articles"]);
        await waitForAfter();
      })(),
    ).resolves.not.toThrow();
  });

  it("fetch が失敗した場合 console.error を呼び出す", async () => {
    const consoleErrorSpy = vi
      .spyOn(console, "error")
      .mockImplementation(() => {});
    mockFetch.mockImplementation(async (input: RequestInfo | URL) => {
      const url = typeof input === "string" ? input : input.toString();
      if (url.startsWith("http://metadata.google.internal")) {
        return new Response(null, { status: 404 });
      }
      throw new Error("Network error");
    });

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    notifyReaderRevalidation(["articles"]);
    await waitForAfter();

    expect(consoleErrorSpy).toHaveBeenCalled();
    consoleErrorSpy.mockRestore();
  });

  it("revalidation が undefined の場合 fetch を呼び出さない", async () => {
    delete process.env.READER_ENDPOINT;
    delete process.env.REVALIDATION_SECRET;

    const { notifyReaderRevalidation } = await import("@/aspects/revalidation");

    notifyReaderRevalidation(["articles"]);
    await waitForAfter();

    expect(mockFetch).not.toHaveBeenCalled();
  });
});
