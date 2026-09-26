/**
 * @vitest-environment node
 */
import { describe, expect, it, vi } from "vitest";
import Page from "../../../src/app/search/page";
import { searchByToken } from "../../../src/actions/search-token";
import { SearchIndex } from "@shared/components/templates/search";

vi.mock("@/actions/search-token", () => ({ searchByToken: vi.fn() }));
vi.mock("@/actions/tag", () => ({
  getAllTags: vi.fn(),
  findAllTags: vi.fn(),
  ofNames: vi.fn(),
}));
vi.mock("@shared/components/templates/search", () => ({ SearchIndex: vi.fn() }));

describe("Reader search page", () => {
  it("Reader 専用検索へ画面を接続する", async () => {
    const element = await Page({
      searchParams: Promise.resolve({ freeWord: "ÉCOLE", type: "article" }),
    });
    expect(element.type).toBe(SearchIndex);
    expect(element.props.search).toBe(searchByToken);
    expect(element.props.unvalidatedCriteria.freeWord).toBe("ÉCOLE");
  });
});
