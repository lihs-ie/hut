/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { MemoMold } from "../../../support/molds/domains/memo/common";
import { TagMold } from "../../../support/molds/domains/attributes/tag";

describe("components/organisms/memo/Memo", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("メモ取得後にJSXを返す", async () => {
    const memo = Forger(MemoMold).forgeWithSeed(1);
    const findBySlug = vi.fn().mockResolvedValue(memo);
    const findAllTags = vi
      .fn()
      .mockResolvedValue(Forger(TagMold).forgeMultiWithSeed(2, 1));
    const renderer = vi.fn().mockReturnValue(null);
    const incrementViewCount = vi.fn().mockResolvedValue(undefined);

    const { Memo } = await import(
      "@shared/components/organisms/memo/index"
    );

    const result = await Memo({
      slug: "test-memo-slug",
      renderer,
      findBySlug,
      findAllTags,
      incrementViewCount,
    });

    expect(findBySlug).toHaveBeenCalledWith("test-memo-slug");
    expect(result).toBeDefined();
    expect(incrementViewCount).not.toHaveBeenCalled();
  });
});
