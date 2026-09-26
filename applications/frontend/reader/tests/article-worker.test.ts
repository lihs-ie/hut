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

  it("reads every page before applying the reader's free-word filter", async () => {
    const fetch = vi.fn(async (request: Request) => {
      const page = new URL(request.url).searchParams.get("page");
      return Response.json({
        articles: page === "1"
          ? Array.from({ length: 100 }, (_, index) => publishedView(
            "Haskell syntax",
            `${identifier.slice(0, -2)}${String(index).padStart(2, "0")}`,
          ))
          : [publishedView("Reader match", `${identifier.slice(0, -2)}A0`)],
        pagination: { total: 101 },
      });
    });
    const repository = articleWorkerRepository({ fetch });

    const articles = await repository.search(criteriaSchema.parse({
      status: PublishStatus.PUBLISHED,
      freeWord: "reader match",
    })).unwrap();

    expect(articles).toHaveLength(1);
    expect(articles[0].title).toBe("Reader match");
    expect(fetch).toHaveBeenCalledTimes(2);
    expect(new URL(fetch.mock.calls[1][0].url).searchParams.get("page")).toBe("2");
  });

  it("does not query the worker for a non-published search", async () => {
    const fetch = vi.fn(async () => Response.json({ articles: [], pagination: { total: 0 } }));
    const articles = await articleWorkerRepository({ fetch })
      .search(criteriaSchema.parse({ status: PublishStatus.DRAFT }))
      .unwrap();

    expect(articles).toEqual([]);
    expect(fetch).not.toHaveBeenCalled();
  });

  it("rejects an incomplete page instead of silently omitting articles", async () => {
    const service: ArticleService = {
      fetch: async () => Response.json({ articles: [], pagination: { total: 1 } }),
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
        });
      }
      return Response.json({ articles: [publishedView("After change")], pagination: { total: 1 } });
    });
    const articles = await articleWorkerRepository({ fetch })
      .search(criteriaSchema.parse({}))
      .unwrap();

    expect(articles).toHaveLength(1);
    expect(articles[0].title).toBe("After change");
    expect(fetch).toHaveBeenCalledTimes(3);
  });

  it("fails after repeated duplicate pages", async () => {
    const fetch = vi.fn(async () => Response.json({
      articles: Array.from({ length: 100 }, () => publishedView()),
      pagination: { total: 101 },
    }));
    const result = await articleWorkerRepository({ fetch })
      .search(criteriaSchema.parse({}))
      .match({ ok: () => null, err: (error) => error });

    expect(isUnexpectedError(result)).toBe(true);
    expect(fetch).toHaveBeenCalledTimes(3);
  });
});
