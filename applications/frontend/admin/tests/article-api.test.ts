/** @vitest-environment node */
import { describe, expect, it, vi } from "vitest";
import { articleAdminApi, ArticleApiError } from "@/infrastructures/article-api";

const article = {
  identifier: "01ARZ3NDEKTSV4RRFFQ69G5FAV",
  phase: "unvalidated",
  title: "Haskell syntax",
  body: "",
  slug: null,
  excerpt: null,
  tags: [],
  images: [],
  createdAt: "2026-01-01T00:00:00Z",
  updatedAt: "2026-01-01T00:00:00Z",
  publishedAt: null,
};

const draft = { title: "Haskell syntax", body: "", slug: null, tags: [] };

describe("Article admin API client", () => {
  it("sends an authenticated draft creation request and validates its response", async () => {
    const fetch = vi.fn(async () => Response.json(article, { status: 201 }));
    const client = articleAdminApi({ fetch }, "https://article.internal", "admin");

    const result = await client.jotDown(draft);

    expect(result.identifier).toBe(article.identifier);
    const request = fetch.mock.calls[0][0] as Request;
    expect(request.method).toBe("POST");
    expect(new URL(request.url).pathname).toBe("/admin/articles");
    expect(request.headers.get("X-Hut-Actor")).toBe("admin");
    expect(await request.json()).toEqual(draft);
  });

  it("uses phase pagination parameters for browsing", async () => {
    const fetch = vi.fn(async () => Response.json({
      articles: [article],
      pagination: { total: 1, items: 1, current: 2, firstPage: 1, lastPage: 2 },
    }));
    const client = articleAdminApi({ fetch }, "https://article.internal", "admin");

    const result = await client.browse(2, 10, "unvalidated");

    expect(result.articles).toHaveLength(1);
    const request = fetch.mock.calls[0][0] as Request;
    expect(new URL(request.url).searchParams.toString())
      .toBe("page=2&size=10&status=unvalidated");
  });

  it("accepts the empty page shape returned by Pager", async () => {
    const client = articleAdminApi({
      fetch: async () => Response.json({
        articles: [],
        pagination: { total: 0, items: 10, current: 1, firstPage: 0, lastPage: 0 },
      }),
    }, "https://article.internal", "admin");

    const result = await client.browse(1, 10);

    expect(result.articles).toEqual([]);
    expect(result.pagination.firstPage).toBe(0);
  });

  it("addresses each lifecycle operation with its own method and route", async () => {
    const fetch = vi.fn(async (request: Request) => {
      const path = new URL(request.url).pathname;
      if (path.endsWith("/proofreading")) {
        return Response.json({ article: article.identifier, phase: "proofreaded" });
      }
      if (path.endsWith("/excerpt-generation-requests")) {
        return Response.json({ article: article.identifier, requestIdentifier: "request" });
      }
      if (request.method === "DELETE" && path.endsWith(article.identifier)) {
        return Response.json({ article: article.identifier });
      }
      if (path.endsWith("/slug-availability")) {
        return Response.json({ available: true });
      }
      return Response.json(article);
    });
    const client = articleAdminApi({ fetch }, "https://article.internal", "admin");
    const identifier = article.identifier;

    await client.view(identifier);
    await client.amendDraft(identifier, draft);
    await client.proofread(identifier);
    await client.reviseExcerpt(identifier, "Edited excerpt");
    await client.publish(identifier);
    await client.takeDown(identifier);
    await client.resumePublication(identifier);
    await client.checkSlug(identifier, "haskell-syntax");
    await client.regenerateExcerpt(identifier);
    await client.discard(identifier);

    expect(fetch.mock.calls.map(([request]) => [request.method, new URL(request.url).pathname]))
      .toEqual([
        ["GET", `/admin/articles/${identifier}`],
        ["PUT", `/admin/articles/${identifier}/draft`],
        ["POST", `/admin/articles/${identifier}/proofreading`],
        ["PATCH", `/admin/articles/${identifier}/excerpt`],
        ["POST", `/admin/articles/${identifier}/publication`],
        ["DELETE", `/admin/articles/${identifier}/publication`],
        ["POST", `/admin/articles/${identifier}/publication-resumptions`],
        ["GET", `/admin/articles/${identifier}/slug-availability`],
        ["POST", `/admin/articles/${identifier}/excerpt-generation-requests`],
        ["DELETE", `/admin/articles/${identifier}`],
      ]);
    expect(new URL(fetch.mock.calls[7][0].url).searchParams.get("slug"))
      .toBe("haskell-syntax");
  });

  it("preserves the API error code and correlation for the presentation layer", async () => {
    const client = articleAdminApi({
      fetch: async () => new Response("Conflict", {
        status: 409,
        headers: {
          "X-Article-Error-Code": "processing_target_changed",
          "X-Correlation-Identifier": "correlation",
        },
      }),
    }, "https://article.internal", "admin");

    await expect(client.publish(article.identifier)).rejects.toMatchObject({
      status: 409,
      code: "processing_target_changed",
      correlation: "correlation",
    } satisfies Partial<ArticleApiError>);
  });

  it("rejects a malformed success response", async () => {
    const client = articleAdminApi({
      fetch: async () => Response.json({ ...article, phase: "unknown" }),
    }, "https://article.internal", "admin");

    await expect(client.view(article.identifier)).rejects.toThrow();
  });
});
