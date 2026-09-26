/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import Page, { revalidate } from "../../src/app/page";
import { latestArticles } from "../../src/actions/feed/article-search";
import { TopIndex } from "@shared/components/templates/top";

vi.mock("@/actions/feed/article-search", () => ({
  latestArticles: vi.fn().mockResolvedValue([]),
  searchMemos: vi.fn().mockResolvedValue([]),
  searchSeries: vi.fn().mockResolvedValue([]),
}));
vi.mock("@/actions/admin", () => ({
  getProfile: vi.fn(),
}));
vi.mock("@/actions/tag", () => ({
  findAllTags: vi.fn(),
}));
vi.mock("@shared/components/templates/top", () => ({
  TopIndex: vi.fn(),
}));

describe("/ (top) page", () => {
  describe("revalidate", () => {
    it("revalidate が 60 でexportされている", () => {
      expect(revalidate).toBe(60);
    });
  });

  it("トップ記事を Reader のページ取得へ接続する", async () => {
    const element = Page();
    expect(element.type).toBe(TopIndex);
    expect(element.props.searchArticles).toBe(latestArticles);
  });
});
