/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../../../support/molds/domains/article/common";
import { TagMold } from "../../../support/molds/domains/attributes/tag";

describe("components/organisms/article/Article", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("記事取得後にJSXを返す", async () => {
    const article = Forger(ArticleMold).forgeWithSeed(1);
    const findBySlug = vi.fn().mockResolvedValue(article);
    const findAllTags = vi
      .fn()
      .mockResolvedValue(Forger(TagMold).forgeMultiWithSeed(2, 1));
    const renderer = vi.fn().mockReturnValue(null);
    const incrementViewCount = vi.fn().mockResolvedValue(undefined);

    const { Article } = await import(
      "@shared/components/organisms/article/index"
    );

    const result = await Article({
      slug: "test-slug",
      renderer,
      findBySlug,
      findAllTags,
      incrementViewCount,
    });

    expect(findBySlug).toHaveBeenCalledWith("test-slug");
    expect(result).toBeDefined();
    expect(incrementViewCount).not.toHaveBeenCalled();
  });
});
