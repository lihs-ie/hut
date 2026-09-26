/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import Page, { revalidate } from "../../../src/app/articles/page";
import { browseArticlesPage } from "../../../src/actions/feed/article-search";
import { ArticleListIndex } from "@shared/components/templates/article/list";
import { renderToStaticMarkup } from "react-dom/server";

vi.mock("@/actions/feed/article-search", () => ({
  browseArticlesPage: vi.fn().mockResolvedValue({ articles: [], total: 21 }),
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

  it("指定ページだけを取得して前後の移動を表示する", async () => {
    const element = await Page({ searchParams: Promise.resolve({ page: "2" }) });
    const [list] = element.props.children;
    expect(list.type).toBe(ArticleListIndex);
    expect(await list.props.search()).toEqual([]);
    expect(browseArticlesPage).toHaveBeenCalledWith(2, 20);
    const html = renderToStaticMarkup(element);
    expect(html).toContain("/articles?page=1");
    expect(html).toContain("2 / 2");
    expect(html).not.toContain("/articles?page=3");
  });

  it("単一ページではページ移動を表示しない", async () => {
    vi.mocked(browseArticlesPage).mockResolvedValueOnce({ articles: [], total: 0 });
    const element = await Page({ searchParams: Promise.resolve({}) });
    expect(renderToStaticMarkup(element)).not.toContain("記事のページ切り替え");
  });
});
