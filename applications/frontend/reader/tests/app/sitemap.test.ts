/**
 * @vitest-environment node
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../../../shared/tests/support/molds/domains/article/common";

vi.mock("react", () => ({
  cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

vi.mock("next/server", () => ({
  connection: vi.fn().mockResolvedValue(undefined),
}));

vi.mock("@/actions/article", () => ({
  searchAllSlugs: vi.fn(),
}));

vi.mock("@/actions/memo", () => ({ searchAllSlugs: vi.fn() }));
vi.mock("@/actions/series", () => ({ searchAllSlugs: vi.fn() }));

describe("sitemap", () => {
  const originalEnv = process.env;

  beforeEach(async () => {
    process.env = { ...originalEnv, NEXT_PUBLIC_SITE_URL: "https://example.com" };
    vi.resetModules();
    const { searchAllSlugs } = await import("@/actions/article");
    vi.mocked(searchAllSlugs).mockResolvedValue([]);
  });

  afterEach(() => {
    process.env = originalEnv;
  });

  it("初期リリースの静的ページだけを含める", async () => {
    const { default: sitemap } = await import("../../src/app/sitemap");
    const urls = (await sitemap()).map((entry) => entry.url);

    expect(urls).toContain("https://example.com/");
    expect(urls).toContain("https://example.com/articles");
    expect(urls).toContain("https://example.com/about");
    expect(urls).toContain("https://example.com/privacy");
    expect(urls).not.toContain("https://example.com/memos");
    expect(urls).not.toContain("https://example.com/series");
  });

  it("記事スラッグだけを動的 URL に含める", async () => {
    const articles = Forger(ArticleMold).forgeMultiWithSeed(3, 1);
    const { searchAllSlugs } = await import("@/actions/article");
    const { searchAllSlugs: searchMemoSlugs } = await import("@/actions/memo");
    const { searchAllSlugs: searchSeriesSlugs } = await import("@/actions/series");
    vi.mocked(searchAllSlugs).mockResolvedValue(articles.map((article) => article.slug));

    const { default: sitemap } = await import("../../src/app/sitemap");
    const urls = (await sitemap()).map((entry) => entry.url);

    for (const article of articles) {
      expect(urls).toContain(`https://example.com/articles/${article.slug}`);
    }
    expect(searchMemoSlugs).not.toHaveBeenCalled();
    expect(searchSeriesSlugs).not.toHaveBeenCalled();
  });

  it("サイト URL 未設定時は既定の URL を使用する", async () => {
    delete process.env.NEXT_PUBLIC_SITE_URL;
    const { default: sitemap } = await import("../../src/app/sitemap");
    const urls = (await sitemap()).map((entry) => entry.url);

    expect(urls).toContain("https://hut.lihs.dev/");
  });
});
