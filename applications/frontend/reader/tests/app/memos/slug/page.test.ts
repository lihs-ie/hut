/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";

vi.mock("react", () => ({
  cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

vi.mock("@/actions/memo", () => ({
  findBySlug: vi.fn(),
  searchAllSlugs: vi.fn().mockResolvedValue(["memo-1", "memo-2"]),
}));

vi.mock("@shared/actions/tag", () => ({
  findAllTags: vi.fn(),
}));

vi.mock("@shared/components/global/mdx", () => ({
  MDXRenderer: vi.fn(),
}));

vi.mock("@shared/components/templates/memo", () => ({
  MemoIndex: vi.fn(),
}));

vi.mock("@shared/actions/view", () => ({
  incrementViewCount: vi.fn(),
}));

describe("/memos/[slug] page", () => {
  describe("revalidate", () => {
    it("revalidate が 3600 でexportされている", async () => {
      const pageModule = await import(
        "@/app/memos/[slug]/page"
      );

      expect(pageModule.revalidate).toBe(3600);
    });
  });

  describe("generateStaticParams", () => {
    it("generateStaticParams が関数としてexportされている", async () => {
      const pageModule = await import(
        "@/app/memos/[slug]/page"
      );

      expect(typeof pageModule.generateStaticParams).toBe("function");
    });

    it("generateStaticParams がslugの配列を返す", async () => {
      const pageModule = await import(
        "@/app/memos/[slug]/page"
      );

      const result = await pageModule.generateStaticParams();

      expect(Array.isArray(result)).toBe(true);
      expect(result).toEqual([
        { slug: "memo-1" },
        { slug: "memo-2" },
      ]);
    });
  });
});
