/** @vitest-environment node */
import { describe, expect, it, vi } from "vitest";
import { isAggregateNotFoundError, isUnexpectedError } from "@shared/aspects/error";
import { criteriaSchema } from "@shared/domains/articles";
import { slugSchema, PublishStatus } from "@shared/domains/common";
import { articleWorkerRepository, ArticleService } from "@/infrastructures/article-worker";

const identifier = "01ARZ3NDEKTSV4RRFFQ69G5FAV";

/** Builds an Article API response with valid published data. */
function publishedView(title = "Haskell syntax", articleIdentifier = identifier) {
  return {
    identifier: articleIdentifier,
    phase: "published",
    title,
    body: "# Syntax\n\nFunctions and types.",
    slug: "haskell-syntax",
    excerpt: "An introduction to Haskell syntax.",
    tags: [],
    images: [],
    createdAt: "2026-01-01T00:00:00Z",
    updatedAt: "2026-01-02T00:00:00Z",
    publishedAt: "2026-01-03T00:00:00Z",
  };
}

describe("Article Worker reader repository", () => {
  it("reads a published article through its private reader route", async () => {
    const fetch = vi.fn(async (_request: Request) => Response.json(publishedView()));
    const repository = articleWorkerRepository({ fetch });

    const article = await repository.findBySlug(slugSchema.parse("haskell-syntax")).unwrap();

    expect(article.identifier).toBe(identifier);
    expect(article.content).toContain("Functions and types");
    expect(article.status).toBe(PublishStatus.PUBLISHED);
    expect(article.publishedAt).toEqual(new Date("2026-01-03T00:00:00Z"));
    expect(new URL(fetch.mock.calls[0][0].url).pathname).toBe("/articles/haskell-syntax");
  });

  it("accepts backend-valid Unicode title and excerpt lengths", async () => {
    const service: ArticleService = {
      fetch: async () => Response.json({
        ...publishedView("😀".repeat(100)),
        excerpt: "😀".repeat(151),
      }),
    };
    const article = await articleWorkerRepository(service)
      .findBySlug(slugSchema.parse("haskell-syntax"))
      .unwrap();

    expect(article.title).toBe("😀".repeat(100));
    expect(article.excerpt).toBe("😀".repeat(151));
  });

  it("maps 404 to ArticleNotFound without requesting an admin route", async () => {
    const service: ArticleService = {
      fetch: async () => new Response(null, { status: 404 }),
    };
    const result = await articleWorkerRepository(service)
      .findBySlug(slugSchema.parse("missing-article"))
      .match({ ok: () => null, err: (error) => error });

    expect(isAggregateNotFoundError(result)).toBe(true);
  });

  it("rejects a non-published response even when the service returns 200", async () => {
    const service: ArticleService = {
      fetch: async () => Response.json({ ...publishedView(), phase: "private" }),
    };
    const result = await articleWorkerRepository(service)
      .findBySlug(slugSchema.parse("haskell-syntax"))
      .match({ ok: () => null, err: (error) => error });

    expect(isUnexpectedError(result)).toBe(true);
  });

  it("asks the Article API to filter a free-word search", async () => {
    const fetch = vi.fn(async (request: Request) => {
      expect(new URL(request.url).searchParams.get("q")).toBe("reader match");
      return Response.json({
        articles: [publishedView("Reader match")],
        pagination: { total: 1 },
        snapshot: "1",
      });
    });
    const repository = articleWorkerRepository({ fetch });

    const articles = await repository.search(criteriaSchema.parse({
      status: PublishStatus.PUBLISHED,
      freeWord: "reader match",
    })).unwrap();

    expect(articles).toHaveLength(1);
    expect(articles[0].title).toBe("Reader match");
    expect(fetch).toHaveBeenCalledTimes(1);
  });

  it("passes all selected tags to the Article API", async () => {
    const first = "01ARZ3NDEKTSV4RRFFQ69G5FAY";
    const second = "01ARZ3NDEKTSV4RRFFQ69G5FAZ";
    const fetch = vi.fn(async (request: Request) => {
      expect(new URL(request.url).searchParams.getAll("tag")).toEqual([first, second]);
      return Response.json({
        articles: [{ ...publishedView(), tags: [second] }],
        pagination: { total: 1 },
        snapshot: "1",
      });
    });
    const articles = await articleWorkerRepository({ fetch })
      .search(criteriaSchema.parse({ tags: [first, second] })).unwrap();

    expect(articles).toHaveLength(1);
    expect(fetch).toHaveBeenCalledTimes(1);
  });

  it("does not query the worker for a non-published search", async () => {
    const fetch = vi.fn(async () => Response.json({ articles: [], pagination: { total: 0 }, snapshot: "0" }));
    const articles = await articleWorkerRepository({ fetch })
      .search(criteriaSchema.parse({ status: PublishStatus.DRAFT }))
      .unwrap();

    expect(articles).toEqual([]);
    expect(fetch).not.toHaveBeenCalled();
  });

  it("uses the single-article route for a slug search", async () => {
    const fetch = vi.fn(async () => Response.json(publishedView()));
    const articles = await articleWorkerRepository({ fetch })
      .search(criteriaSchema.parse({ slug: "haskell-syntax", freeWord: "functions" }))
      .unwrap();

    expect(articles).toHaveLength(1);
    expect(fetch).toHaveBeenCalledTimes(1);
    expect(new URL(fetch.mock.calls[0][0].url).pathname).toBe("/articles/haskell-syntax");
  });

  it("returns an empty slug search when the article is absent or does not match", async () => {
    const missing = articleWorkerRepository({
      fetch: async () => new Response(null, { status: 404 }),
    });
    expect(await missing.search(criteriaSchema.parse({ slug: "missing" })).unwrap()).toEqual([]);

    const unmatched = articleWorkerRepository({
      fetch: async () => Response.json(publishedView()),
    });
    expect(await unmatched.search(criteriaSchema.parse({
      slug: "haskell-syntax",
      freeWord: "unrelated",
    })).unwrap()).toEqual([]);
  });

  it("rejects an incomplete page instead of silently omitting articles", async () => {
    const service: ArticleService = {
      fetch: async () => Response.json({ articles: [], pagination: { total: 1 }, snapshot: "1" }),
    };
    const result = await articleWorkerRepository(service)
      .search(criteriaSchema.parse({}))
      .match({ ok: () => null, err: (error) => error });

    expect(isUnexpectedError(result)).toBe(true);
  });

  it("retries when the total changes between pages", async () => {
    let calls = 0;
    const fetch = vi.fn(async (request: Request) => {
      calls += 1;
      const page = new URL(request.url).searchParams.get("page");
      if (calls <= 2) {
        return Response.json({
          articles: page === "1"
            ? Array.from({ length: 100 }, (_, index) => publishedView(
              "Before change",
              `${identifier.slice(0, -2)}${String(index).padStart(2, "0")}`,
            ))
            : [],
          pagination: { total: page === "1" ? 101 : 100 },
          snapshot: String(calls),
        });
      }
      return Response.json({ articles: [publishedView("After change")], pagination: { total: 1 }, snapshot: "3" });
    });
    const articles = await articleWorkerRepository({ fetch })
      .search(criteriaSchema.parse({}))
      .unwrap();

    expect(articles).toHaveLength(1);
    expect(articles[0].title).toBe("After change");
    expect(fetch).toHaveBeenCalledTimes(3);
  });

  it("retries when articles change without changing the page total", async () => {
    let calls = 0;
    const fetch = vi.fn(async (request: Request) => {
      calls += 1;
      const page = new URL(request.url).searchParams.get("page");
      if (calls <= 2) {
        return Response.json({
          articles: page === "1"
            ? Array.from({ length: 100 }, (_, index) => publishedView(
              "Before change",
              `${identifier.slice(0, -2)}${String(index).padStart(2, "0")}`,
            ))
            : [publishedView("Replacement", `${identifier.slice(0, -2)}A0`)],
          pagination: { total: 101 },
          snapshot: String(calls),
        });
      }
      return Response.json({
        articles: [publishedView("After change")],
        pagination: { total: 1 },
        snapshot: "2",
      });
    });
    const articles = await articleWorkerRepository({ fetch })
      .search(criteriaSchema.parse({})).unwrap();

    expect(articles).toHaveLength(1);
    expect(articles[0].title).toBe("After change");
    expect(fetch).toHaveBeenCalledTimes(3);
  });

  it("retries a page whose snapshot changed during its backend read", async () => {
    const fetch = vi.fn()
      .mockResolvedValueOnce(new Response(null, {
        status: 503,
        headers: { "X-Article-Error-Code": "snapshot_changed" },
      }))
      .mockResolvedValueOnce(Response.json({
        articles: [publishedView()],
        pagination: { total: 1 },
        snapshot: "2",
      }));
    const articles = await articleWorkerRepository({ fetch })
      .search(criteriaSchema.parse({})).unwrap();

    expect(articles).toHaveLength(1);
    expect(fetch).toHaveBeenCalledTimes(2);
  });

  it("fails after repeated duplicate pages", async () => {
    const fetch = vi.fn(async () => Response.json({
      articles: Array.from({ length: 100 }, () => publishedView()),
      pagination: { total: 101 },
      snapshot: "1",
    }));
    const result = await articleWorkerRepository({ fetch })
      .search(criteriaSchema.parse({}))
      .match({ ok: () => null, err: (error) => error });

    expect(isUnexpectedError(result)).toBe(true);
    expect(fetch).toHaveBeenCalledTimes(3);
  });
});
