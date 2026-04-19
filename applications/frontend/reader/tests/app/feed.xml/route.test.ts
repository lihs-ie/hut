/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../../../../shared/tests/support/molds/domains/article/common";
import { MemoMold } from "../../../../shared/tests/support/molds/domains/memo/common";
import { SeriesMold } from "../../../../shared/tests/support/molds/domains/series/common";
import { ChapterMold } from "../../../../shared/tests/support/molds/domains/series/chapter";

vi.mock("react", () => ({
  cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

vi.mock("@/actions/article", () => ({
  searchAllSlugs: vi.fn(),
}));

vi.mock("@/actions/memo", () => ({
  searchAllSlugs: vi.fn(),
}));

vi.mock("@/actions/feed/article-search", () => ({
  searchArticles: vi.fn(),
  searchMemos: vi.fn(),
  searchSeries: vi.fn(),
}));

vi.mock("@/actions/chapter", () => ({
  findPublishedChaptersByIdentifiers: vi.fn(),
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

  const setEmptyDefaults = async () => {
    const { searchArticles, searchMemos, searchSeries } = await import(
      "@/actions/feed/article-search"
    );
    const { findPublishedChaptersByIdentifiers } = await import(
      "@/actions/chapter"
    );

    vi.mocked(searchArticles).mockResolvedValue([]);
    vi.mocked(searchMemos).mockResolvedValue([]);
    vi.mocked(searchSeries).mockResolvedValue([]);
    vi.mocked(findPublishedChaptersByIdentifiers).mockResolvedValue([]);
  };

  it("Content-Type: application/xml を返す", async () => {
    await setEmptyDefaults();
    process.env.NEXT_PUBLIC_SITE_URL = "https://example.com";

    const { GET } = await import("../../../src/app/feed.xml/route");
    const response = await GET();

    expect(response.headers.get("Content-Type")).toContain("application/rss+xml");
  });

  it("記事がfeedに含まれる", async () => {
    await setEmptyDefaults();
    const articles = Forger(ArticleMold).forgeMultiWithSeed(2, 1);
    const { searchArticles } = await import("@/actions/feed/article-search");
    vi.mocked(searchArticles).mockResolvedValue(articles);

    process.env.NEXT_PUBLIC_SITE_URL = "https://example.com";

    const { GET } = await import("../../../src/app/feed.xml/route");
    const response = await GET();
    const text = await response.text();

    for (const article of articles) {
      expect(text).toContain(article.title);
    }
  });

  it("メモがfeedに含まれる", async () => {
    await setEmptyDefaults();
    const memos = Forger(MemoMold).forgeMultiWithSeed(2, 1);
    const { searchMemos } = await import("@/actions/feed/article-search");
    vi.mocked(searchMemos).mockResolvedValue(memos);

    process.env.NEXT_PUBLIC_SITE_URL = "https://example.com";

    const { GET } = await import("../../../src/app/feed.xml/route");
    const response = await GET();
    const text = await response.text();

    for (const memo of memos) {
      expect(text).toContain(memo.title);
    }
  });

  it("連載がfeedに含まれる", async () => {
    await setEmptyDefaults();
    const seriesList = Forger(SeriesMold).forgeMultiWithSeed(2, 1);
    const { searchSeries } = await import("@/actions/feed/article-search");
    vi.mocked(searchSeries).mockResolvedValue(seriesList);

    process.env.NEXT_PUBLIC_SITE_URL = "https://example.com";

    const { GET } = await import("../../../src/app/feed.xml/route");
    const response = await GET();
    const text = await response.text();

    for (const series of seriesList) {
      expect(text).toContain(series.title);
      expect(text).toContain(`/series/${series.slug}`);
    }
  });

  it("チャプターがfeedに含まれる", async () => {
    await setEmptyDefaults();
    const seriesList = Forger(SeriesMold).forgeMultiWithSeed(1, 10);
    const series = seriesList[0];
    const chapters = Forger(ChapterMold).forgeMultiWithSeed(2, 20);

    const { searchSeries } = await import("@/actions/feed/article-search");
    const { findPublishedChaptersByIdentifiers } = await import(
      "@/actions/chapter"
    );
    vi.mocked(searchSeries).mockResolvedValue(seriesList);
    vi.mocked(findPublishedChaptersByIdentifiers).mockResolvedValue(chapters);

    process.env.NEXT_PUBLIC_SITE_URL = "https://example.com";

    const { GET } = await import("../../../src/app/feed.xml/route");
    const response = await GET();
    const text = await response.text();

    for (const chapter of chapters) {
      expect(text).toContain(chapter.title);
      expect(text).toContain(
        `/series/${series.slug}/chapters/${chapter.slug}`,
      );
    }
  });

  it("有効なXML構造を返す", async () => {
    await setEmptyDefaults();
    process.env.NEXT_PUBLIC_SITE_URL = "https://example.com";

    const { GET } = await import("../../../src/app/feed.xml/route");
    const response = await GET();
    const text = await response.text();

    expect(text).toContain("<?xml");
    expect(text).toContain("<rss");
    expect(text).toContain("</rss>");
  });
});
