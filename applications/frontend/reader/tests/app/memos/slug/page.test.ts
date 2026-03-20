/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import {
  revalidate,
  generateStaticParams,
} from "../../../../src/app/memos/[slug]/page";

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
        { slug: "memo-1" },
        { slug: "memo-2" },
      ]);
    });
  });
});
