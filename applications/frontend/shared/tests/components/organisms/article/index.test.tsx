/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../../../support/molds/domains/article/common";
import { TagMold } from "../../../support/molds/domains/attributes/tag";
import { ContentType } from "@shared/domains/search-token/reference";

describe("components/organisms/article/Article", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("記事取得後にincrementViewCountが呼ばれる", async () => {
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

    await Article({
      slug: "test-slug",
      renderer,
      findBySlug,
      findAllTags,
      incrementViewCount,
    });

    expect(findBySlug).toHaveBeenCalledWith("test-slug");
    expect(incrementViewCount).toHaveBeenCalledTimes(1);
    expect(incrementViewCount).toHaveBeenCalledWith({
      type: ContentType.ARTICLE,
      content: article.identifier,
    });
  });

  it("incrementViewCountがエラーを投げても記事表示はブロックされない", async () => {
    const article = Forger(ArticleMold).forgeWithSeed(2);
    const findBySlug = vi.fn().mockResolvedValue(article);
    const findAllTags = vi
      .fn()
      .mockResolvedValue(Forger(TagMold).forgeMultiWithSeed(2, 2));
    const renderer = vi.fn().mockReturnValue(null);
    const incrementViewCount = vi
      .fn()
      .mockRejectedValue(new Error("Firestore error"));

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

    expect(result).toBeDefined();
    expect(incrementViewCount).toHaveBeenCalledTimes(1);
  });
});
