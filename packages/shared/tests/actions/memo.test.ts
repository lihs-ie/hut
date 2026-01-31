/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  MemoMold,
  MemoIdentifierMold,
  MemoSlugMold,
  MemoEntryMold,
} from "../support/molds/domains/memo/common";
import { aggregateNotFoundError } from "@shared/aspects/error";
import {
  createSuccessAsyncResult,
  createErrorAsyncResult,
  setupUnwrapForAsyncResult,
  setupUnwrapWithResolvedValue,
  setupUnwrapWithRejectedValue,
} from "../support/helpers";

vi.mock("react", () => ({
  cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

vi.mock("@shared/components/global/next-error", () => ({
  unwrapForNextJs: vi.fn(),
}));

const mockFind = vi.fn();
const mockFindBySlug = vi.fn();
const mockSearch = vi.fn();

vi.mock("@shared/providers/workflows/memo", () => ({
  MemoWorkflowProvider: {
    find: mockFind,
    findBySlug: mockFindBySlug,
    search: mockSearch,
  },
}));

describe("memo actions", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockFind.mockReset();
    mockFindBySlug.mockReset();
    mockSearch.mockReset();
  });

  describe("find", () => {
    it("identifierでメモを正常に取得できる", async () => {
      const memo = Forger(MemoMold).forgeWithSeed(1);
      const unwrapForNextJs = await setupUnwrapWithResolvedValue(memo);

      const { find } = await import("@shared/actions/memo");
      const result = await find(memo.identifier);

      expect(result).toEqual(memo);
      expect(unwrapForNextJs).toHaveBeenCalled();
    });

    it("メモが見つからない場合はエラーがスローされる", async () => {
      const identifier = Forger(MemoIdentifierMold).forgeWithSeed(1);
      await setupUnwrapWithRejectedValue(
        aggregateNotFoundError("Memo", "Memo not found.")
      );

      const { find } = await import("@shared/actions/memo");

      await expect(find(identifier)).rejects.toThrow();
    });
  });

  describe("findBySlug", () => {
    it("slugでメモを正常に取得できる", async () => {
      const memo = Forger(MemoMold).forgeWithSeed(2);
      const unwrapForNextJs = await setupUnwrapWithResolvedValue(memo);

      const { findBySlug } = await import("@shared/actions/memo");
      const result = await findBySlug(memo.slug);

      expect(result).toEqual(memo);
      expect(unwrapForNextJs).toHaveBeenCalled();
    });

    it("slugに対応するメモが見つからない場合はエラーがスローされる", async () => {
      const slug = Forger(MemoSlugMold).forgeWithSeed(1);
      await setupUnwrapWithRejectedValue(
        aggregateNotFoundError("Memo", "Memo not found.")
      );

      const { findBySlug } = await import("@shared/actions/memo");

      await expect(findBySlug(slug)).rejects.toThrow();
    });
  });

  describe("search", () => {
    it("検索条件に一致するメモ一覧を取得できる", async () => {
      const memos = Forger(MemoMold).forgeMultiWithSeed(3, 1);
      const unwrapForNextJs = await setupUnwrapWithResolvedValue(memos);

      const { search } = await import("@shared/actions/memo");
      const result = await search({ tags: null, freeWord: null, status: null });

      expect(result).toEqual(memos);
      expect(unwrapForNextJs).toHaveBeenCalled();
    });

    it("検索条件に一致するメモがない場合は空配列を返す", async () => {
      await setupUnwrapWithResolvedValue([]);

      const { search } = await import("@shared/actions/memo");
      const result = await search({ tags: null, freeWord: "non-existent", status: null });

      expect(result).toEqual([]);
    });
  });

  describe("getEntriesBySlug", () => {
    it("slugでメモのエントリ一覧を正常に取得できる", async () => {
      const entries = Forger(MemoEntryMold).forgeMultiWithSeed(3, 1);
      const memo = Forger(MemoMold).forgeWithSeed(3, { entries });
      mockFindBySlug.mockReturnValue(createSuccessAsyncResult(memo));
      await setupUnwrapForAsyncResult();

      const { getEntriesBySlug } = await import("@shared/actions/memo");
      const result = await getEntriesBySlug(memo.slug);

      expect(result).toEqual(entries);
      expect(mockFindBySlug).toHaveBeenCalledWith(memo.slug);
    });

    it("メモが見つからない場合はエラーがスローされる", async () => {
      const slug = Forger(MemoSlugMold).forgeWithSeed(1);
      mockFindBySlug.mockReturnValue(
        createErrorAsyncResult(aggregateNotFoundError("Memo", "Memo not found."))
      );
      await setupUnwrapForAsyncResult();

      const { getEntriesBySlug } = await import("@shared/actions/memo");

      await expect(getEntriesBySlug(slug)).rejects.toThrow();
    });
  });
});
