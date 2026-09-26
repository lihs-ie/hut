/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import Page from "../../src/app/page";
import { latestArticles } from "../../src/actions/feed/article-search";
import { TopIndex } from "@shared/components/templates/top";
import { connection } from "next/server";

vi.mock("next/server", () => ({ connection: vi.fn().mockResolvedValue(undefined) }));

vi.mock("@/actions/feed/article-search", () => ({
  latestArticles: vi.fn().mockResolvedValue([]),
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
  it("トップ記事を Reader のページ取得へ接続する", async () => {
    const element = await Page();
    expect(connection).toHaveBeenCalled();
    expect(element.type).toBe(TopIndex);
    expect(element.props.searchArticles).toBe(latestArticles);
  });
});
