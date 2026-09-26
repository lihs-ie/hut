/**
 * @vitest-environment node
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../../../../shared/tests/support/molds/domains/article/common";

vi.mock("react", () => ({
  cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

vi.mock("@/actions/feed/article-search", () => ({
  searchArticles: vi.fn(),
}));

describe("GET /feed.xml", () => {
  const originalEnv = process.env;

  beforeEach(async () => {
    process.env = { ...originalEnv, NEXT_PUBLIC_SITE_URL: "https://example.com" };
    vi.resetModules();
    const { searchArticles } = await import("@/actions/feed/article-search");
    vi.mocked(searchArticles).mockResolvedValue([]);
  });

  afterEach(() => {
    process.env = originalEnv;
  });

  it("RSS の Content-Type と XML 構造を返す", async () => {
    const { GET } = await import("../../../src/app/feed.xml/route");
    const response = await GET();
    const xml = await response.text();

    expect(response.headers.get("Content-Type")).toContain("application/rss+xml");
    expect(xml).toContain("<?xml");
    expect(xml).toContain("<rss");
    expect(xml).toContain("</rss>");
  });

  it("記事だけを feed に含める", async () => {
    const articles = Forger(ArticleMold).forgeMultiWithSeed(2, 1);
    const { searchArticles } = await import(
      "@/actions/feed/article-search"
    );
    vi.mocked(searchArticles).mockResolvedValue(articles);

    const { GET } = await import("../../../src/app/feed.xml/route");
    const xml = await (await GET()).text();

    for (const article of articles) {
      expect(xml).toContain(article.title);
      expect(xml).toContain(`/articles/${article.slug}`);
    }
    expect(xml).not.toContain("/memos/");
    expect(xml).not.toContain("/series/");
  });
});
