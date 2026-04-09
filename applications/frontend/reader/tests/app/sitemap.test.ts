/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../../../shared/tests/support/molds/domains/article/common";
import { MemoMold } from "../../../shared/tests/support/molds/domains/memo/common";
import { SeriesMold } from "../../../shared/tests/support/molds/domains/series/common";

vi.mock("react", () => ({
  cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

vi.mock("@/actions/article", () => ({
  searchAllSlugs: vi.fn(),
}));

vi.mock("@/actions/memo", () => ({
  searchAllSlugs: vi.fn(),
}));

vi.mock("@/actions/series", () => ({
  searchAllSlugs: vi.fn(),
}));

describe("sitemap", () => {
  const originalEnv = process.env;

  beforeEach(() => {
    process.env = { ...originalEnv };
    vi.resetModules();
  });

  afterEach(() => {
    process.env = originalEnv;
  });

  it("静的ページが含まれる", async () => {
    const { searchAllSlugs: searchArticleSlugs } = await import("@/actions/article");
    const { searchAllSlugs: searchMemoSlugs } = await import("@/actions/memo");
    const { searchAllSlugs: searchSeriesSlugs } = await import("@/actions/series");

    vi.mocked(searchArticleSlugs).mockResolvedValue([]);
    vi.mocked(searchMemoSlugs).mockResolvedValue([]);
    vi.mocked(searchSeriesSlugs).mockResolvedValue([]);

    process.env.NEXT_PUBLIC_SITE_URL = "https://example.com";

    const { default: sitemap } = await import("../../src/app/sitemap");
    const result = await sitemap();

    const urls = result.map((entry) => entry.url);
    expect(urls).toContain("https://example.com/");
    expect(urls).toContain("https://example.com/articles");
    expect(urls).toContain("https://example.com/memos");
    expect(urls).toContain("https://example.com/series");
    expect(urls).toContain("https://example.com/about");
    expect(urls).toContain("https://example.com/privacy");
  });

  it("記事スラッグが動的URLとして含まれる", async () => {
    const articles = Forger(ArticleMold).forgeMultiWithSeed(3, 1);
    const { searchAllSlugs: searchArticleSlugs } = await import("@/actions/article");
    const { searchAllSlugs: searchMemoSlugs } = await import("@/actions/memo");
    const { searchAllSlugs: searchSeriesSlugs } = await import("@/actions/series");

    vi.mocked(searchArticleSlugs).mockResolvedValue(articles.map((a) => a.slug));
    vi.mocked(searchMemoSlugs).mockResolvedValue([]);
    vi.mocked(searchSeriesSlugs).mockResolvedValue([]);

    process.env.NEXT_PUBLIC_SITE_URL = "https://example.com";

    const { default: sitemap } = await import("../../src/app/sitemap");
    const result = await sitemap();

    const urls = result.map((entry) => entry.url);
    for (const article of articles) {
      expect(urls).toContain(`https://example.com/articles/${article.slug}`);
    }
  });

  it("メモスラッグが動的URLとして含まれる", async () => {
    const memos = Forger(MemoMold).forgeMultiWithSeed(2, 1);
    const { searchAllSlugs: searchArticleSlugs } = await import("@/actions/article");
    const { searchAllSlugs: searchMemoSlugs } = await import("@/actions/memo");
    const { searchAllSlugs: searchSeriesSlugs } = await import("@/actions/series");

    vi.mocked(searchArticleSlugs).mockResolvedValue([]);
    vi.mocked(searchMemoSlugs).mockResolvedValue(memos.map((m) => m.slug));
    vi.mocked(searchSeriesSlugs).mockResolvedValue([]);

    process.env.NEXT_PUBLIC_SITE_URL = "https://example.com";

    const { default: sitemap } = await import("../../src/app/sitemap");
    const result = await sitemap();

    const urls = result.map((entry) => entry.url);
    for (const memo of memos) {
      expect(urls).toContain(`https://example.com/memos/${memo.slug}`);
    }
  });

  it("シリーズスラッグが動的URLとして含まれる", async () => {
    const seriesList = Forger(SeriesMold).forgeMultiWithSeed(2, 1);
    const { searchAllSlugs: searchArticleSlugs } = await import("@/actions/article");
    const { searchAllSlugs: searchMemoSlugs } = await import("@/actions/memo");
    const { searchAllSlugs: searchSeriesSlugs } = await import("@/actions/series");

    vi.mocked(searchArticleSlugs).mockResolvedValue([]);
    vi.mocked(searchMemoSlugs).mockResolvedValue([]);
    vi.mocked(searchSeriesSlugs).mockResolvedValue(seriesList.map((s) => s.slug));

    process.env.NEXT_PUBLIC_SITE_URL = "https://example.com";

    const { default: sitemap } = await import("../../src/app/sitemap");
    const result = await sitemap();

    const urls = result.map((entry) => entry.url);
    for (const series of seriesList) {
      expect(urls).toContain(`https://example.com/series/${series.slug}`);
    }
  });

  it("NEXT_PUBLIC_SITE_URL が未設定の場合はフォールバックURLを使用する", async () => {
    const { searchAllSlugs: searchArticleSlugs } = await import("@/actions/article");
    const { searchAllSlugs: searchMemoSlugs } = await import("@/actions/memo");
    const { searchAllSlugs: searchSeriesSlugs } = await import("@/actions/series");

    vi.mocked(searchArticleSlugs).mockResolvedValue([]);
    vi.mocked(searchMemoSlugs).mockResolvedValue([]);
    vi.mocked(searchSeriesSlugs).mockResolvedValue([]);

    delete process.env.NEXT_PUBLIC_SITE_URL;

    const { default: sitemap } = await import("../../src/app/sitemap");
    const result = await sitemap();

    expect(result.length).toBeGreaterThan(0);
    for (const entry of result) {
      expect(entry.url).toMatch(/^https?:\/\//);
    }
  });
});
