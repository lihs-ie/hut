import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  createMemoFindWorkflow,
  createMemoFindBySlugWorkflow,
  createMemoSearchWorkflow,
  createMemoCreateWorkflow,
  createMemoEditWorkflow,
  createMemoTerminateWorkflow,
} from "@shared/workflows/memo";
import {
  validateMemoIdentifier,
  validateMemo,
  validateCriteria,
  toSnapshot,
} from "@shared/domains/memo";
import { validateSlug } from "@shared/domains/common/slug";
import { Logger, Environment } from "@shared/aspects/logger";
import { ok, err } from "@shared/aspects/result";
import {
  aggregateNotFoundError,
  isAggregateNotFoundError,
  unexpectedError,
} from "@shared/aspects/error";
import { PublishStatus } from "@shared/domains/common";
import {
  createPassthroughFilter,
  createPublishedOnlyFilter,
} from "@shared/workflows/common";
import {
  MemoMold,
  MemoIdentifierMold,
  MemoSlugMold,
} from "../support/molds/domains/memo";
import { Command } from "@shared/workflows/common";

describe("workflows/memo", () => {
  const mockLogger = Logger(Environment.DEVELOPMENT);

  beforeEach(() => {
    vi.spyOn(console, "log").mockImplementation(() => {});
  });

  describe("createMemoFindWorkflow", () => {
    it("有効なidentifierでメモを取得できる", async () => {
      const memo = Forger(MemoMold).forgeWithSeed(1);
      const findMock = vi.fn().mockReturnValue(ok(memo).toAsync());

      const workflow = createMemoFindWorkflow(validateMemoIdentifier)(findMock)(
        mockLogger
      );

      const result = await workflow(memo.identifier).unwrap();

      expect(result).toEqual(memo);
      expect(findMock).toHaveBeenCalledWith(memo.identifier);
    });

    it("無効なidentifierでValidationErrorを返す", async () => {
      const findMock = vi.fn();

      const workflow = createMemoFindWorkflow(validateMemoIdentifier)(findMock)(
        mockLogger
      );

      const result = workflow("invalid-identifier");

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(findMock).not.toHaveBeenCalled();
    });

    it("メモが見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const identifier = Forger(MemoIdentifierMold).forgeWithSeed(1);
      const notFoundError = aggregateNotFoundError("Memo", "Memo not found");
      const findMock = vi.fn().mockReturnValue(err(notFoundError).toAsync());

      const workflow = createMemoFindWorkflow(validateMemoIdentifier)(findMock)(
        mockLogger
      );

      const result = await workflow(identifier).unwrapError();

      expect(result).toEqual(notFoundError);
    });
  });

  describe("createMemoFindBySlugWorkflow", () => {
    it("有効なslugでメモを取得できる", async () => {
      const memo = Forger(MemoMold).forgeWithSeed(1);
      const findBySlugMock = vi.fn().mockReturnValue(ok(memo).toAsync());

      const workflow = createMemoFindBySlugWorkflow(validateSlug)(
        findBySlugMock
      )(createPassthroughFilter())(mockLogger);

      const result = await workflow(memo.slug).unwrap();

      expect(result).toEqual(memo);
      expect(findBySlugMock).toHaveBeenCalledWith(memo.slug);
    });

    it("無効なslugでValidationErrorを返す", async () => {
      const findBySlugMock = vi.fn();

      const workflow = createMemoFindBySlugWorkflow(validateSlug)(
        findBySlugMock
      )(createPassthroughFilter())(mockLogger);

      const result = workflow("Invalid Slug With Spaces");

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(findBySlugMock).not.toHaveBeenCalled();
    });

    it("メモが見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const slug = Forger(MemoSlugMold).forgeWithSeed(1);
      const notFoundError = aggregateNotFoundError("Memo", "Memo not found");
      const findBySlugMock = vi
        .fn()
        .mockReturnValue(err(notFoundError).toAsync());

      const workflow = createMemoFindBySlugWorkflow(validateSlug)(
        findBySlugMock
      )(createPassthroughFilter())(mockLogger);

      const result = await workflow(slug).unwrapError();

      expect(result).toEqual(notFoundError);
    });

    it("createPublishedOnlyFilterでdraft状態のメモはAggregateNotFoundErrorを返す", async () => {
      const memo = Forger(MemoMold).forgeWithSeed(1, {
        status: PublishStatus.DRAFT,
      });
      const findBySlugMock = vi.fn().mockReturnValue(ok(memo).toAsync());

      const workflow = createMemoFindBySlugWorkflow(validateSlug)(
        findBySlugMock
      )(createPublishedOnlyFilter("Memo"))(mockLogger);

      const error = await workflow(memo.slug).unwrapError();

      expect(isAggregateNotFoundError(error)).toBe(true);
    });

    it("createPublishedOnlyFilterでarchived状態のメモはAggregateNotFoundErrorを返す", async () => {
      const memo = Forger(MemoMold).forgeWithSeed(1, {
        status: PublishStatus.ARCHIVED,
      });
      const findBySlugMock = vi.fn().mockReturnValue(ok(memo).toAsync());

      const workflow = createMemoFindBySlugWorkflow(validateSlug)(
        findBySlugMock
      )(createPublishedOnlyFilter("Memo"))(mockLogger);

      const error = await workflow(memo.slug).unwrapError();

      expect(isAggregateNotFoundError(error)).toBe(true);
    });

    it("createPublishedOnlyFilterでpublished状態のメモは正常に返す", async () => {
      const memo = Forger(MemoMold).forgeWithSeed(1, {
        status: PublishStatus.PUBLISHED,
      });
      const findBySlugMock = vi.fn().mockReturnValue(ok(memo).toAsync());

      const workflow = createMemoFindBySlugWorkflow(validateSlug)(
        findBySlugMock
      )(createPublishedOnlyFilter("Memo"))(mockLogger);

      const result = await workflow(memo.slug).unwrap();

      expect(result).toEqual(memo);
    });
  });

  describe("createMemoSearchWorkflow", () => {
    it("有効な検索条件でメモを検索できる", async () => {
      const memos = Forger(MemoMold).forgeMultiWithSeed(3, 1);
      const searchMock = vi.fn().mockReturnValue(ok(memos).toAsync());

      const workflow = createMemoSearchWorkflow(validateCriteria)(searchMock)(
        mockLogger
      );

      const command: Command<{
        tags: string[] | null;
        freeWord: string | null;
        status: string | null;
      }> = {
        now: new Date(),
        payload: { tags: null, freeWord: "test", status: null },
      };

      const result = await workflow(command).unwrap();

      expect(result).toEqual(memos);
      expect(searchMock).toHaveBeenCalled();
    });

    it("nullの検索条件でも検索できる", async () => {
      const memos = Forger(MemoMold).forgeMultiWithSeed(5, 1);
      const searchMock = vi.fn().mockReturnValue(ok(memos).toAsync());

      const workflow = createMemoSearchWorkflow(validateCriteria)(searchMock)(
        mockLogger
      );

      const command: Command<{
        tags: null;
        freeWord: null;
        status: null;
      }> = {
        now: new Date(),
        payload: { tags: null, freeWord: null, status: null },
      };

      const result = await workflow(command).unwrap();

      expect(result).toEqual(memos);
    });

    it("検索でエラーが発生した場合はUnexpectedErrorを返す", async () => {
      const error = unexpectedError("Search failed");
      const searchMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createMemoSearchWorkflow(validateCriteria)(searchMock)(
        mockLogger
      );

      const command: Command<{
        tags: null;
        freeWord: null;
        status: null;
      }> = {
        now: new Date(),
        payload: { tags: null, freeWord: null, status: null },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createMemoCreateWorkflow", () => {
    it("有効なメモデータでメモを作成できる", async () => {
      const memo = Forger(MemoMold).forgeWithSeed(1);
      const persistMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createMemoCreateWorkflow(validateMemo)(persistMock)(
        mockLogger
      );

      const command: Command<{
        unvalidated: {
          identifier: string;
          title: string;
          slug: string;
          entries: Array<{ text: string; createdAt: Date }>;
          tags: string[];
          images: string[];
          status: string;
          timeline: { createdAt: Date; updatedAt: Date };
        };
      }> = {
        now: new Date(),
        payload: {
          unvalidated: {
            identifier: memo.identifier,
            title: memo.title,
            slug: memo.slug,
            entries: memo.entries,
            tags: memo.tags,
            images: memo.images,
            status: memo.status,
            timeline: memo.timeline,
          },
        },
      };

      const result = await workflow(command).unwrap();

      expect(result.type).toBe("memo.created");
      expect(result.payload.snapshot.identifier).toBe(memo.identifier);
      expect(persistMock).toHaveBeenCalled();
    });

    it("無効なメモデータでValidationErrorを返す", async () => {
      const persistMock = vi.fn();

      const workflow = createMemoCreateWorkflow(validateMemo)(persistMock)(
        mockLogger
      );

      const command: Command<{
        unvalidated: {
          identifier: string;
          title: string;
          slug: string;
          entries: Array<{ text: string; createdAt: Date }>;
          tags: string[];
          images: string[];
          status: string;
          timeline: { createdAt: Date; updatedAt: Date };
        };
      }> = {
        now: new Date(),
        payload: {
          unvalidated: {
            identifier: "invalid",
            title: "", // 空のタイトルは無効
            slug: "test-slug",
            entries: [],
            tags: [],
            images: [],
            status: "published",
            timeline: { createdAt: new Date(), updatedAt: new Date() },
          },
        },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(persistMock).not.toHaveBeenCalled();
    });

    it("永続化でエラーが発生した場合はエラーを返す", async () => {
      const memo = Forger(MemoMold).forgeWithSeed(1);
      const error = unexpectedError("Persist failed");
      const persistMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createMemoCreateWorkflow(validateMemo)(persistMock)(
        mockLogger
      );

      const command: Command<{
        unvalidated: {
          identifier: string;
          title: string;
          slug: string;
          entries: Array<{ text: string; createdAt: Date }>;
          tags: string[];
          images: string[];
          status: string;
          timeline: { createdAt: Date; updatedAt: Date };
        };
      }> = {
        now: new Date(),
        payload: {
          unvalidated: {
            identifier: memo.identifier,
            title: memo.title,
            slug: memo.slug,
            entries: memo.entries,
            tags: memo.tags,
            images: memo.images,
            status: memo.status,
            timeline: memo.timeline,
          },
        },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createMemoEditWorkflow", () => {
    it("有効なメモデータでメモを編集できる", async () => {
      const memo = Forger(MemoMold).forgeWithSeed(1);
      const before = toSnapshot(memo);
      const persistMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createMemoEditWorkflow(validateMemo)(persistMock)(
        mockLogger
      );

      const updatedTitle = "Updated Title";
      const command: Command<{
        unvalidated: {
          identifier: string;
          title: string;
          slug: string;
          entries: Array<{ text: string; createdAt: Date }>;
          tags: string[];
          images: string[];
          status: string;
          timeline: { createdAt: Date; updatedAt: Date };
        };
        before: typeof before;
      }> = {
        now: new Date(),
        payload: {
          unvalidated: {
            identifier: memo.identifier,
            title: updatedTitle,
            slug: memo.slug,
            entries: memo.entries,
            tags: memo.tags,
            images: memo.images,
            status: memo.status,
            timeline: memo.timeline,
          },
          before,
        },
      };

      const result = await workflow(command).unwrap();

      expect(result.type).toBe("memo.edited");
      expect(result.payload.before).toEqual(before);
      expect(persistMock).toHaveBeenCalled();
    });

    it("無効なメモデータでValidationErrorを返す", async () => {
      const memo = Forger(MemoMold).forgeWithSeed(1);
      const before = toSnapshot(memo);
      const persistMock = vi.fn();

      const workflow = createMemoEditWorkflow(validateMemo)(persistMock)(
        mockLogger
      );

      const command: Command<{
        unvalidated: {
          identifier: string;
          title: string;
          slug: string;
          entries: Array<{ text: string; createdAt: Date }>;
          tags: string[];
          images: string[];
          status: string;
          timeline: { createdAt: Date; updatedAt: Date };
        };
        before: typeof before;
      }> = {
        now: new Date(),
        payload: {
          unvalidated: {
            identifier: "invalid",
            title: "",
            slug: "test-slug",
            entries: [],
            tags: [],
            images: [],
            status: "published",
            timeline: { createdAt: new Date(), updatedAt: new Date() },
          },
          before,
        },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(persistMock).not.toHaveBeenCalled();
    });
  });

  describe("createMemoTerminateWorkflow", () => {
    it("有効なidentifierでメモを削除できる", async () => {
      const identifier = Forger(MemoIdentifierMold).forgeWithSeed(1);
      const terminateMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createMemoTerminateWorkflow(validateMemoIdentifier)(
        terminateMock
      )(mockLogger);

      const command: Command<{ identifier: string }> = {
        now: new Date(),
        payload: { identifier },
      };

      const result = await workflow(command).unwrap();

      expect(result.type).toBe("memo.terminated");
      expect(terminateMock).toHaveBeenCalledWith(identifier);
    });

    it("無効なidentifierでValidationErrorを返す", async () => {
      const terminateMock = vi.fn();

      const workflow = createMemoTerminateWorkflow(validateMemoIdentifier)(
        terminateMock
      )(mockLogger);

      const command: Command<{ identifier: string }> = {
        now: new Date(),
        payload: { identifier: "invalid-identifier" },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(terminateMock).not.toHaveBeenCalled();
    });

    it("メモが見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const identifier = Forger(MemoIdentifierMold).forgeWithSeed(1);
      const notFoundError = aggregateNotFoundError("Memo", "Memo not found");
      const terminateMock = vi
        .fn()
        .mockReturnValue(err(notFoundError).toAsync());

      const workflow = createMemoTerminateWorkflow(validateMemoIdentifier)(
        terminateMock
      )(mockLogger);

      const command: Command<{ identifier: string }> = {
        now: new Date(),
        payload: { identifier },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(notFoundError);
    });
  });
});
