/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import {
  createFirestoreRestClient,
  type FirestoreRestClient,
} from "@/infrastructures/firestore-rest/client";
import type { AccessTokenProvider } from "@/infrastructures/firestore-rest/access-token";

const createMockResponse = (body: unknown, status: number = 200): Response => {
  return new Response(JSON.stringify(body), {
    status,
    headers: { "Content-Type": "application/json" },
  });
};

const createEmptyResponse = (status: number): Response => {
  return new Response(null, { status });
};

const createAccessTokenProviderStub = (token: string): AccessTokenProvider => ({
  getAccessToken: async () => token,
});

describe("infrastructures/firestore-rest/client", () => {
  const projectId = "demo-hut";
  let originalFetch: typeof fetch;
  let client: FirestoreRestClient;

  beforeEach(() => {
    originalFetch = globalThis.fetch;
    client = createFirestoreRestClient({
      projectId,
      accessTokenProvider: createAccessTokenProviderStub("token-xyz"),
    });
  });

  afterEach(() => {
    globalThis.fetch = originalFetch;
  });

  describe("getDocument", () => {
    it("指定したパスのドキュメントをオブジェクトに変換して返す", async () => {
      const mockFetch = vi.fn().mockResolvedValue(
        createMockResponse({
          name: `projects/${projectId}/databases/(default)/documents/articles/doc-1`,
          fields: {
            identifier: { stringValue: "doc-1" },
            title: { stringValue: "タイトル" },
          },
        }),
      );
      globalThis.fetch = mockFetch;

      const result = await client.getDocument("articles/doc-1");

      expect(result).toEqual({
        identifier: "doc-1",
        title: "タイトル",
      });
      expect(mockFetch).toHaveBeenCalledWith(
        `https://firestore.googleapis.com/v1/projects/${projectId}/databases/(default)/documents/articles/doc-1`,
        expect.objectContaining({
          method: "GET",
          headers: expect.objectContaining({
            Authorization: "Bearer token-xyz",
          }),
        }),
      );
    });

    it("404 の場合は null を返す", async () => {
      const mockFetch = vi.fn().mockResolvedValue(
        createMockResponse(
          { error: { code: 404, message: "not found" } },
          404,
        ),
      );
      globalThis.fetch = mockFetch;

      const result = await client.getDocument("articles/missing");

      expect(result).toBeNull();
    });

    it("401 の場合はエラーをスローする", async () => {
      const mockFetch = vi.fn().mockResolvedValue(
        createEmptyResponse(401),
      );
      globalThis.fetch = mockFetch;

      await expect(client.getDocument("articles/any")).rejects.toThrow();
    });

    it("500 の場合はエラーをスローする", async () => {
      const mockFetch = vi.fn().mockResolvedValue(
        createMockResponse(
          { error: { code: 500, message: "internal" } },
          500,
        ),
      );
      globalThis.fetch = mockFetch;

      await expect(client.getDocument("articles/any")).rejects.toThrow();
    });
  });

  describe("runQuery", () => {
    it("POST で structured query を送信する", async () => {
      const mockFetch = vi.fn().mockResolvedValue(
        createMockResponse([
          {
            document: {
              name: `projects/${projectId}/databases/(default)/documents/articles/doc-1`,
              fields: {
                identifier: { stringValue: "doc-1" },
                title: { stringValue: "タイトル1" },
              },
            },
          },
          {
            document: {
              name: `projects/${projectId}/databases/(default)/documents/articles/doc-2`,
              fields: {
                identifier: { stringValue: "doc-2" },
                title: { stringValue: "タイトル2" },
              },
            },
          },
        ]),
      );
      globalThis.fetch = mockFetch;

      const documents = await client.runQuery({
        collectionId: "articles",
        where: [
          { field: "status", op: "EQUAL", value: { stringValue: "published" } },
        ],
      });

      expect(documents).toHaveLength(2);
      expect(documents[0]).toEqual({
        identifier: "doc-1",
        title: "タイトル1",
      });
      expect(mockFetch).toHaveBeenCalledWith(
        `https://firestore.googleapis.com/v1/projects/${projectId}/databases/(default)/documents:runQuery`,
        expect.objectContaining({
          method: "POST",
          headers: expect.objectContaining({
            Authorization: "Bearer token-xyz",
            "Content-Type": "application/json",
          }),
        }),
      );
    });

    it("ドキュメントが無い結果を空配列として返す", async () => {
      const mockFetch = vi.fn().mockResolvedValue(
        createMockResponse([{ readTime: "2024-01-01T00:00:00Z" }]),
      );
      globalThis.fetch = mockFetch;

      const documents = await client.runQuery({
        collectionId: "articles",
        where: [],
      });

      expect(documents).toEqual([]);
    });

    it("orderBy を指定して送信できる", async () => {
      const mockFetch = vi
        .fn()
        .mockResolvedValue(createMockResponse([]));
      globalThis.fetch = mockFetch;

      await client.runQuery({
        collectionId: "articles",
        where: [],
        orderBy: [{ field: "timeline.createdAt", direction: "DESCENDING" }],
      });

      const fetchCall = mockFetch.mock.calls[0];
      const requestInit = fetchCall[1] as RequestInit;
      const bodyString = typeof requestInit.body === "string" ? requestInit.body : "";
      const parsed = JSON.parse(bodyString) as {
        structuredQuery: { orderBy: unknown };
      };

      expect(parsed.structuredQuery.orderBy).toEqual([
        {
          field: { fieldPath: "timeline.createdAt" },
          direction: "DESCENDING",
        },
      ]);
    });

    it("500 の場合はエラーをスローする", async () => {
      const mockFetch = vi
        .fn()
        .mockResolvedValue(createMockResponse({ error: "oops" }, 500));
      globalThis.fetch = mockFetch;

      await expect(
        client.runQuery({ collectionId: "articles", where: [] }),
      ).rejects.toThrow();
    });
  });

  describe("emulator mode", () => {
    it("baseUrl を指定すると emulator エンドポイントを叩く", async () => {
      const mockFetch = vi.fn().mockResolvedValue(
        createMockResponse({
          name: `projects/${projectId}/databases/(default)/documents/articles/doc-1`,
          fields: { identifier: { stringValue: "doc-1" } },
        }),
      );
      globalThis.fetch = mockFetch;

      const emulatorClient = createFirestoreRestClient({
        projectId,
        baseUrl: "http://127.0.0.1:8085/v1",
      });

      await emulatorClient.getDocument("articles/doc-1");

      expect(mockFetch).toHaveBeenCalledWith(
        `http://127.0.0.1:8085/v1/projects/${projectId}/databases/(default)/documents/articles/doc-1`,
        expect.objectContaining({
          method: "GET",
          headers: expect.objectContaining({
            Authorization: "Bearer owner",
          }),
        }),
      );
    });

    it("accessTokenProvider を省略すると owner トークンで runQuery を送信する", async () => {
      const mockFetch = vi.fn().mockResolvedValue(createMockResponse([]));
      globalThis.fetch = mockFetch;

      const emulatorClient = createFirestoreRestClient({
        projectId,
        baseUrl: "http://127.0.0.1:8085/v1",
      });

      await emulatorClient.runQuery({
        collectionId: "articles",
        where: [],
      });

      expect(mockFetch).toHaveBeenCalledWith(
        `http://127.0.0.1:8085/v1/projects/${projectId}/databases/(default)/documents:runQuery`,
        expect.objectContaining({
          method: "POST",
          headers: expect.objectContaining({
            Authorization: "Bearer owner",
          }),
        }),
      );
    });
  });
});
