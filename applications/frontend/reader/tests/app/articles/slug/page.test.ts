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

vi.mock("@shared/actions/tag", () => ({
  findAllTags: vi.fn(),
}));

vi.mock("@shared/components/global/mdx", () => ({
  MDXRenderer: vi.fn(),
  generateToc: vi.fn(),
}));

vi.mock("@shared/components/templates/article", () => ({
  ArticleIndex: vi.fn(),
}));

vi.mock("@shared/actions/view", () => ({
  incrementViewCount: vi.fn(),
}));

vi.mock("@/actions/article", () => ({
  findBySlug: vi.fn(),
  createTableOfContents: vi.fn(),
  searchAllSlugs: vi.fn().mockResolvedValue(["slug-1", "slug-2", "slug-3"]),
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

    it("generateStaticParams がslugの配列を返す", async () => {
      const result = await generateStaticParams();

      expect(Array.isArray(result)).toBe(true);
      expect(result).toEqual([
        { slug: "slug-1" },
        { slug: "slug-2" },
        { slug: "slug-3" },
      ]);
    });
  });
});
