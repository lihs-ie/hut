/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import Page, { revalidate } from "../../src/app/page";
import { searchArticles } from "../../src/actions/feed/article-search";
import { TopIndex } from "@shared/components/templates/top";

vi.mock("@/actions/feed/article-search", () => ({
  searchArticles: vi.fn().mockResolvedValue([]),
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

  it("Article の検索を Reader 専用アクションへ接続する", async () => {
    const element = Page();
    expect(element.type).toBe(TopIndex);
    await element.props.searchArticles();
    expect(searchArticles).toHaveBeenCalledWith({});
  });
});
