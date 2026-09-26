/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import {
  revalidate,
  generateStaticParams,
} from "../../../../src/app/articles/[slug]/page";

vi.mock("react", () => ({
  cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

vi.mock("@/actions/tag", () => ({
  findAllTags: vi.fn(),
}));

vi.mock("@shared/components/global/mdx", () => ({
  MDXRenderer: vi.fn(),
  generateTOC: vi.fn(),
}));

vi.mock("@shared/components/templates/article", () => ({
  ArticleIndex: vi.fn(),
}));

vi.mock("@/actions/view", () => ({
  incrementViewCount: vi.fn(),
}));

vi.mock("@/actions/article", () => ({
  findBySlug: vi.fn(),
  createTableOfContents: vi.fn(),
}));

describe("/articles/[slug] page", () => {
  describe("revalidate", () => {
    it("revalidate が 3600 でexportされている", () => {
      expect(revalidate).toBe(3600);
    });
  });

  describe("generateStaticParams", () => {
    it("generateStaticParams が関数としてexportされている", () => {
      expect(typeof generateStaticParams).toBe("function");
    });

    it("ビルド時には記事の slug を取得しない", async () => {
      const result = await generateStaticParams();
      expect(result).toEqual([]);
    });
  });
});
