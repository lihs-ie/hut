/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  TagMold,
  TagIdentifierMold,
} from "../support/molds/domains/attributes/tag";
import { aggregateNotFoundError } from "@shared/aspects/error";
import {
  createSuccessAsyncResult,
  createErrorAsyncResult,
  setupUnwrapForAsyncResult,
  setupUnwrapWithResolvedValue,
} from "../support/helpers";

vi.mock("react", () => ({
  cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

vi.mock("@shared/components/global/next-error", () => ({
  unwrapForNextJs: vi.fn(),
}));

const mockOfIdentifiers = vi.fn();
const mockSearch = vi.fn();
const mockOfNames = vi.fn();

vi.mock("@shared/providers/workflows/tag", () => ({
  TagWorkflowProvider: {
    ofIdentifiers: mockOfIdentifiers,
    search: mockSearch,
    ofNames: mockOfNames,
  },
}));

describe("tag actions", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockOfIdentifiers.mockReset();
    mockSearch.mockReset();
    mockOfNames.mockReset();
  });

  describe("find", () => {
    it("identifierでタグを正常に取得できる", async () => {
      const tag = Forger(TagMold).forgeWithSeed(1);
      mockOfIdentifiers.mockReturnValue(createSuccessAsyncResult([tag]));
      await setupUnwrapForAsyncResult();

      const { find } = await import("@shared/actions/tag");
      const result = await find(tag.identifier);

      expect(result).toEqual(tag);
      expect(mockOfIdentifiers).toHaveBeenCalledWith([tag.identifier]);
    });

    it("タグが見つからない場合はエラーがスローされる", async () => {
      const identifier = Forger(TagIdentifierMold).forgeWithSeed(1);
      mockOfIdentifiers.mockReturnValue(
        createErrorAsyncResult(aggregateNotFoundError("Tag", "Tag not found."))
      );
      await setupUnwrapForAsyncResult();

      const { find } = await import("@shared/actions/tag");

      await expect(find(identifier)).rejects.toThrow();
    });
  });

  describe("findAllTags", () => {
    it("複数のidentifierでタグ一覧を正常に取得できる", async () => {
      const tags = Forger(TagMold).forgeMultiWithSeed(3, 1);
      const unwrapForNextJs = await setupUnwrapWithResolvedValue(tags);

      const { findAllTags } = await import("@shared/actions/tag");
      const result = await findAllTags(tags.map((tag) => tag.identifier));

      expect(result).toEqual(tags);
      expect(unwrapForNextJs).toHaveBeenCalled();
    });

    it("空の配列を渡した場合は空配列を返す", async () => {
      await setupUnwrapWithResolvedValue([]);

      const { findAllTags } = await import("@shared/actions/tag");
      const result = await findAllTags([]);

      expect(result).toEqual([]);
    });
  });

  describe("getAllTags", () => {
    it("全てのタグを正常に取得できる", async () => {
      const tags = Forger(TagMold).forgeMultiWithSeed(5, 1);
      const unwrapForNextJs = await setupUnwrapWithResolvedValue(tags);

      const { getAllTags } = await import("@shared/actions/tag");
      const result = await getAllTags();

      expect(result).toEqual(tags);
      expect(unwrapForNextJs).toHaveBeenCalled();
    });

    it("タグがない場合は空配列を返す", async () => {
      await setupUnwrapWithResolvedValue([]);

      const { getAllTags } = await import("@shared/actions/tag");
      const result = await getAllTags();

      expect(result).toEqual([]);
    });
  });

  describe("search", () => {
    it("検索条件に一致するタグ一覧を取得できる", async () => {
      const tags = Forger(TagMold).forgeMultiWithSeed(3, 1);
      const unwrapForNextJs = await setupUnwrapWithResolvedValue(tags);

      const { search } = await import("@shared/actions/tag");
      const result = await search({ name: null });

      expect(result).toEqual(tags);
      expect(unwrapForNextJs).toHaveBeenCalled();
    });

    it("nameで検索できる", async () => {
      const tags = Forger(TagMold).forgeMultiWithSeed(2, 1);
      await setupUnwrapWithResolvedValue(tags);

      const { search } = await import("@shared/actions/tag");
      const result = await search({ name: "test" });

      expect(result).toEqual(tags);
    });

    it("検索条件に一致するタグがない場合は空配列を返す", async () => {
      await setupUnwrapWithResolvedValue([]);

      const { search } = await import("@shared/actions/tag");
      const result = await search({ name: "non-existent" });

      expect(result).toEqual([]);
    });
  });

  describe("ofNames", () => {
    it("name一覧からタグ一覧を正常に取得できる", async () => {
      const tags = Forger(TagMold).forgeMultiWithSeed(3, 1);
      const unwrapForNextJs = await setupUnwrapWithResolvedValue(tags);

      const { ofNames } = await import("@shared/actions/tag");
      const result = await ofNames(tags.map((tag) => tag.name));

      expect(result).toEqual(tags);
      expect(unwrapForNextJs).toHaveBeenCalled();
    });

    it("空の配列を渡した場合は空配列を返す", async () => {
      await setupUnwrapWithResolvedValue([]);

      const { ofNames } = await import("@shared/actions/tag");
      const result = await ofNames([]);

      expect(result).toEqual([]);
    });
  });
});
