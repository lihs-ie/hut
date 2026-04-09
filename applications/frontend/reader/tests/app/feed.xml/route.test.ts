/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../../../../shared/tests/support/molds/domains/article/common";
import { MemoMold } from "../../../../shared/tests/support/molds/domains/memo/common";

vi.mock("react", () => ({
  cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

vi.mock("@/actions/article", () => ({
  searchAllSlugs: vi.fn(),
}));

vi.mock("@/actions/memo", () => ({
  searchAllSlugs: vi.fn(),
}));

vi.mock("@shared/actions/article", () => ({
  search: vi.fn(),
}));

vi.mock("@shared/actions/memo", () => ({
  search: vi.fn(),
}));

describe("GET /feed.xml", () => {
  const originalEnv = process.env;

  beforeEach(() => {
    process.env = { ...originalEnv };
    vi.resetModules();
  });

  afterEach(() => {
    process.env = originalEnv;
  });

  it("Content-Type: application/xml を返す", async () => {
    const { search: searchArticles } = await import("@shared/actions/article");
    const { search: searchMemos } = await import("@shared/actions/memo");

    vi.mocked(searchArticles).mockResolvedValue([]);
    vi.mocked(searchMemos).mockResolvedValue([]);

    process.env.NEXT_PUBLIC_SITE_URL = "https://example.com";

    const { GET } = await import("../../../src/app/feed.xml/route");
    const response = await GET();

    expect(response.headers.get("Content-Type")).toContain("application/xml");
  });

  it("記事がfeedに含まれる", async () => {
    const articles = Forger(ArticleMold).forgeMultiWithSeed(2, 1);
    const { search: searchArticles } = await import("@shared/actions/article");
    const { search: searchMemos } = await import("@shared/actions/memo");

    vi.mocked(searchArticles).mockResolvedValue(articles);
    vi.mocked(searchMemos).mockResolvedValue([]);

    process.env.NEXT_PUBLIC_SITE_URL = "https://example.com";

    const { GET } = await import("../../../src/app/feed.xml/route");
    const response = await GET();
    const text = await response.text();

    for (const article of articles) {
      expect(text).toContain(article.title);
    }
  });

  it("メモがfeedに含まれる", async () => {
    const memos = Forger(MemoMold).forgeMultiWithSeed(2, 1);
    const { search: searchArticles } = await import("@shared/actions/article");
    const { search: searchMemos } = await import("@shared/actions/memo");

    vi.mocked(searchArticles).mockResolvedValue([]);
    vi.mocked(searchMemos).mockResolvedValue(memos);

    process.env.NEXT_PUBLIC_SITE_URL = "https://example.com";

    const { GET } = await import("../../../src/app/feed.xml/route");
    const response = await GET();
    const text = await response.text();

    for (const memo of memos) {
      expect(text).toContain(memo.title);
    }
  });

  it("有効なXML構造を返す", async () => {
    const { search: searchArticles } = await import("@shared/actions/article");
    const { search: searchMemos } = await import("@shared/actions/memo");

    vi.mocked(searchArticles).mockResolvedValue([]);
    vi.mocked(searchMemos).mockResolvedValue([]);

    process.env.NEXT_PUBLIC_SITE_URL = "https://example.com";

    const { GET } = await import("../../../src/app/feed.xml/route");
    const response = await GET();
    const text = await response.text();

    expect(text).toContain("<?xml");
    expect(text).toContain("<rss");
    expect(text).toContain("</rss>");
  });
});
