/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { NextRequest } from "next/server";

const mockRevalidateTag = vi.fn();

vi.mock("next/cache", () => ({
  revalidateTag: mockRevalidateTag,
}));

describe("POST /api/revalidate", () => {
  const originalEnv = process.env;

  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    process.env = {
      ...originalEnv,
      REVALIDATION_SECRET: "test-secret-token",
    };
  });

  afterEach(() => {
    process.env = originalEnv;
  });

  describe("認証", () => {
    it("正しいシークレットトークンがある場合は 200 を返す", async () => {
      const { POST } = await import("@/app/api/revalidate/route");
      const request = new NextRequest("http://localhost/api/revalidate", {
        method: "POST",
        headers: {
          "x-revalidation-secret": "test-secret-token",
          "content-type": "application/json",
        },
        body: JSON.stringify({ tags: ["articles"] }),
      });

      const response = await POST(request);

      expect(response.status).toBe(200);
    });

    it("シークレットトークンがない場合は 401 を返す", async () => {
      const { POST } = await import("@/app/api/revalidate/route");
      const request = new NextRequest("http://localhost/api/revalidate", {
        method: "POST",
        headers: {
          "content-type": "application/json",
        },
        body: JSON.stringify({ tags: ["articles"] }),
      });

      const response = await POST(request);

      expect(response.status).toBe(401);
    });

    it("シークレットトークンが不正な場合は 401 を返す", async () => {
      const { POST } = await import("@/app/api/revalidate/route");
      const request = new NextRequest("http://localhost/api/revalidate", {
        method: "POST",
        headers: {
          "x-revalidation-secret": "wrong-secret",
          "content-type": "application/json",
        },
        body: JSON.stringify({ tags: ["articles"] }),
      });

      const response = await POST(request);

      expect(response.status).toBe(401);
    });
  });

  describe("リクエストボディ検証", () => {
    it("tags が配列でない場合は 400 を返す", async () => {
      const { POST } = await import("@/app/api/revalidate/route");
      const request = new NextRequest("http://localhost/api/revalidate", {
        method: "POST",
        headers: {
          "x-revalidation-secret": "test-secret-token",
          "content-type": "application/json",
        },
        body: JSON.stringify({ tags: "articles" }),
      });

      const response = await POST(request);

      expect(response.status).toBe(400);
    });

    it("tags が存在しない場合は 400 を返す", async () => {
      const { POST } = await import("@/app/api/revalidate/route");
      const request = new NextRequest("http://localhost/api/revalidate", {
        method: "POST",
        headers: {
          "x-revalidation-secret": "test-secret-token",
          "content-type": "application/json",
        },
        body: JSON.stringify({}),
      });

      const response = await POST(request);

      expect(response.status).toBe(400);
    });

    it("tags の要素が文字列でない場合は 400 を返す", async () => {
      const { POST } = await import("@/app/api/revalidate/route");
      const request = new NextRequest("http://localhost/api/revalidate", {
        method: "POST",
        headers: {
          "x-revalidation-secret": "test-secret-token",
          "content-type": "application/json",
        },
        body: JSON.stringify({ tags: [123] }),
      });

      const response = await POST(request);

      expect(response.status).toBe(400);
    });

    it("tags が空配列の場合は 200 を返す", async () => {
      const { POST } = await import("@/app/api/revalidate/route");
      const request = new NextRequest("http://localhost/api/revalidate", {
        method: "POST",
        headers: {
          "x-revalidation-secret": "test-secret-token",
          "content-type": "application/json",
        },
        body: JSON.stringify({ tags: [] }),
      });

      const response = await POST(request);

      expect(response.status).toBe(200);
    });
  });

  describe("revalidateTag 呼び出し", () => {
    it("指定されたタグで revalidateTag を呼び出す", async () => {
      const { POST } = await import("@/app/api/revalidate/route");
      const request = new NextRequest("http://localhost/api/revalidate", {
        method: "POST",
        headers: {
          "x-revalidation-secret": "test-secret-token",
          "content-type": "application/json",
        },
        body: JSON.stringify({ tags: ["articles"] }),
      });

      await POST(request);

      expect(mockRevalidateTag).toHaveBeenCalledWith("articles");
    });

    it("複数タグが指定された場合は全タグで revalidateTag を呼び出す", async () => {
      const { POST } = await import("@/app/api/revalidate/route");
      const request = new NextRequest("http://localhost/api/revalidate", {
        method: "POST",
        headers: {
          "x-revalidation-secret": "test-secret-token",
          "content-type": "application/json",
        },
        body: JSON.stringify({ tags: ["articles", "series", "chapters"] }),
      });

      await POST(request);

      expect(mockRevalidateTag).toHaveBeenCalledWith("articles");
      expect(mockRevalidateTag).toHaveBeenCalledWith("series");
      expect(mockRevalidateTag).toHaveBeenCalledWith("chapters");
      expect(mockRevalidateTag).toHaveBeenCalledTimes(3);
    });

    it("revalidateTag 後にレスポンスボディに revalidated: true が含まれる", async () => {
      const { POST } = await import("@/app/api/revalidate/route");
      const request = new NextRequest("http://localhost/api/revalidate", {
        method: "POST",
        headers: {
          "x-revalidation-secret": "test-secret-token",
          "content-type": "application/json",
        },
        body: JSON.stringify({ tags: ["articles"] }),
      });

      const response = await POST(request);
      const body = await response.json();

      expect(body.revalidated).toBe(true);
    });
  });

  describe("環境変数が未設定の場合", () => {
    it("REVALIDATION_SECRET が未設定の場合は全リクエストを 401 で拒否する", async () => {
      delete process.env.REVALIDATION_SECRET;

      const { POST } = await import("@/app/api/revalidate/route");
      const request = new NextRequest("http://localhost/api/revalidate", {
        method: "POST",
        headers: {
          "x-revalidation-secret": "any-token",
          "content-type": "application/json",
        },
        body: JSON.stringify({ tags: ["articles"] }),
      });

      const response = await POST(request);

      expect(response.status).toBe(401);
    });
  });
});
