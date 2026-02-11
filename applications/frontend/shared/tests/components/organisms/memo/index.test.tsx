/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { MemoMold } from "../../../support/molds/domains/memo/common";
import { TagMold } from "../../../support/molds/domains/attributes/tag";
import { ContentType } from "@shared/domains/search-token/reference";

describe("components/organisms/memo/Memo", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("メモ取得後にincrementViewCountが呼ばれる", async () => {
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

    await Memo({
      slug: "test-memo-slug",
      renderer,
      findBySlug,
      findAllTags,
      incrementViewCount,
    });

    expect(findBySlug).toHaveBeenCalledWith("test-memo-slug");
    expect(incrementViewCount).toHaveBeenCalledTimes(1);
    expect(incrementViewCount).toHaveBeenCalledWith({
      type: ContentType.MEMO,
      content: memo.identifier,
    });
  });

  it("incrementViewCountがエラーを投げてもメモ表示はブロックされない", async () => {
    const memo = Forger(MemoMold).forgeWithSeed(2);
    const findBySlug = vi.fn().mockResolvedValue(memo);
    const findAllTags = vi
      .fn()
      .mockResolvedValue(Forger(TagMold).forgeMultiWithSeed(2, 2));
    const renderer = vi.fn().mockReturnValue(null);
    const incrementViewCount = vi
      .fn()
      .mockRejectedValue(new Error("Firestore error"));

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

    expect(result).toBeDefined();
    expect(incrementViewCount).toHaveBeenCalledTimes(1);
  });
});
