/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import Page, { revalidate } from "../../../src/app/articles/page";
import { searchArticles } from "../../../src/actions/feed/article-search";
import { ArticleListIndex } from "@shared/components/templates/article/list";

vi.mock("@/actions/feed/article-search", () => ({
  searchArticles: vi.fn().mockResolvedValue([]),
}));
vi.mock("@/actions/tag", () => ({
  findAllTags: vi.fn(),
}));
vi.mock("@shared/components/templates/article/list", () => ({
  ArticleListIndex: vi.fn(),
}));

describe("/articles page", () => {
  describe("revalidate", () => {
    it("revalidate が 60 でexportされている", () => {
      expect(revalidate).toBe(60);
    });
  });

  it("記事一覧を Reader 専用アクションへ接続する", async () => {
    const element = Page();
    expect(element.type).toBe(ArticleListIndex);
    await element.props.search();
    expect(searchArticles).toHaveBeenCalledWith({});
  });
});
