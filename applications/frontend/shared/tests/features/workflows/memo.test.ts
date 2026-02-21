/**
 * Memo Workflow Feature Test
 *
 * Firebase Emulatorを使用してMemoワークフローの統合テストを行います。
 */

import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  createMemoFindWorkflow,
  createMemoFindBySlugWorkflow,
  createMemoSearchWorkflow,
  createMemoCreateWorkflow,
  createMemoTerminateWorkflow,
} from "@shared/workflows/memo";
import {
  validateMemo,
  validateMemoIdentifier,
  validateCriteria,
} from "@shared/domains/memo";
import { validateSlug } from "@shared/domains/common/slug";
import { PublishStatus } from "@shared/domains/common";
import { createPassthroughFilter } from "@shared/workflows/common";
import { FirebaseMemoRepository } from "@shared/infrastructures/memos";
import {
  createFeatureTestContext,
  cleanupFeatureTest,
  testLogger,
  type FeatureTestContext,
} from "../setup";
import {
  MemoMold,
  MemoIdentifierMold,
  MemoSlugMold,
  MemoTitleMold,
} from "../../support/molds/domains/memo";
import type { Memo, MemoRepository } from "@shared/domains/memo";

function toMemoPayload(memo: Memo) {
  return {
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
  };
}

describe("Feature: Memo Workflow (実DB接続)", () => {
  let context: FeatureTestContext;
  let repository: MemoRepository;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});

    context = await createFeatureTestContext();
    repository = FirebaseMemoRepository(context.firestore, context.operations);
  }, 30000);

  afterAll(async () => {
    await cleanupFeatureTest();
  }, 30000);

  describe("メモ作成から検索までの一連の流れ", () => {
    it("新規メモを作成し、そのメモを検索して取得できる", async () => {
      const createWorkflow = createMemoCreateWorkflow(validateMemo)(
        repository.persist
      )(testLogger);
      const searchWorkflow = createMemoSearchWorkflow(validateCriteria)(
        repository.search
      )(testLogger);

      const memo = Forger(MemoMold).forgeWithSeed(1, {
        status: PublishStatus.PUBLISHED,
      });

      const createResult = await createWorkflow({
        now: new Date(),
        payload: toMemoPayload(memo),
      }).unwrap();

      expect(createResult.payload.snapshot.identifier).toBe(memo.identifier);
      expect(createResult.occurredAt).toBeDefined();

      const searchResult = await searchWorkflow({
        now: new Date(),
        payload: { tags: null, freeWord: null, status: null },
      }).unwrap();

      expect(searchResult.length).toBeGreaterThanOrEqual(1);
      expect(
        searchResult.some((item) => item.identifier === memo.identifier)
      ).toBe(true);
    });

    it("メモを作成後、identifierで取得できる", async () => {
      const createWorkflow = createMemoCreateWorkflow(validateMemo)(
        repository.persist
      )(testLogger);
      const findWorkflow = createMemoFindWorkflow(validateMemoIdentifier)(
        repository.find
      )(testLogger);

      const memo = Forger(MemoMold).forgeWithSeed(2);

      await createWorkflow({
        now: new Date(),
        payload: toMemoPayload(memo),
      }).unwrap();

      const result = await findWorkflow(memo.identifier).unwrap();

      expect(result.identifier).toBe(memo.identifier);
      expect(result.title).toBe(memo.title);
    });
  });

  describe("メモ検索ワークフロー", () => {
    it("identifierでメモを取得できる", async () => {
      const memo = Forger(MemoMold).forgeWithSeed(10);

      await repository.persist(memo).unwrap();

      const findWorkflow = createMemoFindWorkflow(validateMemoIdentifier)(
        repository.find
      )(testLogger);

      const result = await findWorkflow(memo.identifier).unwrap();

      expect(result.identifier).toBe(memo.identifier);
      expect(result.title).toBe(memo.title);
    });

    it("slugでメモを取得できる", async () => {
      const slug = Forger(MemoSlugMold).forgeWithSeed(20, {
        value: "feature-test-memo-slug",
      });
      const memo = Forger(MemoMold).forgeWithSeed(21, { slug });

      await repository.persist(memo).unwrap();

      const findBySlugWorkflow = createMemoFindBySlugWorkflow(validateSlug)(
        repository.findBySlug
      )(createPassthroughFilter())(testLogger);

      const result = await findBySlugWorkflow(slug).unwrap();

      expect(result.identifier).toBe(memo.identifier);
      expect(result.slug).toBe(slug);
    });

    it("フリーワードでメモを検索できる", async () => {
      const targetKeyword = "feature-test-keyword";
      const memo1 = Forger(MemoMold).forgeWithSeed(30, {
        title: Forger(MemoTitleMold).forgeWithSeed(30, {
          value: `Memo with ${targetKeyword}`,
        }),
        status: PublishStatus.PUBLISHED,
      });
      const memo2 = Forger(MemoMold).forgeWithSeed(31, {
        title: Forger(MemoTitleMold).forgeWithSeed(31, {
          value: "Another memo",
        }),
        status: PublishStatus.PUBLISHED,
      });

      await repository.persist(memo1).unwrap();
      await repository.persist(memo2).unwrap();

      const searchWorkflow = createMemoSearchWorkflow(validateCriteria)(
        repository.search
      )(testLogger);

      const result = await searchWorkflow({
        now: new Date(),
        payload: { tags: null, freeWord: targetKeyword, status: null },
      }).unwrap();

      expect(result.length).toBe(1);
      expect(result[0]?.identifier).toBe(memo1.identifier);
    });
  });

  describe("メモ削除ワークフロー", () => {
    it("メモを削除でき、その後検索で見つからない", async () => {
      const memo = Forger(MemoMold).forgeWithSeed(40);

      await repository.persist(memo).unwrap();

      const terminateWorkflow = createMemoTerminateWorkflow(
        validateMemoIdentifier
      )(repository.terminate)(testLogger);
      const findWorkflow = createMemoFindWorkflow(validateMemoIdentifier)(
        repository.find
      )(testLogger);

      const terminateResult = await terminateWorkflow({
        now: new Date(),
        payload: { identifier: memo.identifier },
      }).unwrap();

      expect(terminateResult.payload.memo).toBe(memo.identifier);
      expect(terminateResult.occurredAt).toBeDefined();

      const findResult = await findWorkflow(memo.identifier).match({
        ok: () => "found",
        err: () => "not-found",
      });

      expect(findResult).toBe("not-found");
    });
  });

  describe("並列処理の最適化", () => {
    it("複数メモの作成が並列で実行できる", async () => {
      const createWorkflow = createMemoCreateWorkflow(validateMemo)(
        repository.persist
      )(testLogger);

      const memos = Forger(MemoMold).forgeMultiWithSeed(3, 50);

      const results = await Promise.all(
        memos.map((memo) =>
          createWorkflow({
            now: new Date(),
            payload: toMemoPayload(memo),
          }).unwrap()
        )
      );

      expect(results.length).toBe(3);
      results.forEach((result, index) => {
        expect(result.payload.snapshot.identifier).toBe(
          memos[index]?.identifier
        );
      });

      // 永続化されたことを確認
      for (const memo of memos) {
        const found = await repository.find(memo.identifier).unwrap();
        expect(found.identifier).toBe(memo.identifier);
      }
    });
  });

  describe("エラーハンドリング", () => {
    it("存在しないメモを検索するとAggregateNotFoundErrorが返る", async () => {
      const findWorkflow = createMemoFindWorkflow(validateMemoIdentifier)(
        repository.find
      )(testLogger);

      const nonExistentIdentifier =
        Forger(MemoIdentifierMold).forgeWithSeed(100);

      const result = await findWorkflow(nonExistentIdentifier).match({
        ok: () => ({ found: true }),
        err: (error) => ({ found: false, error }),
      });

      expect(result.found).toBe(false);
    });

    it("無効なidentifierでValidationErrorが返る", async () => {
      const findWorkflow = createMemoFindWorkflow(validateMemoIdentifier)(
        repository.find
      )(testLogger);

      const result = await findWorkflow("invalid-identifier").match({
        ok: () => ({ valid: true }),
        err: () => ({ valid: false }),
      });

      expect(result.valid).toBe(false);
    });
  });

  describe("データ永続化の検証", () => {
    it("作成したメモのデータが正確に永続化される", async () => {
      const memo = Forger(MemoMold).forgeWithSeed(110, {
        status: PublishStatus.PUBLISHED,
      });

      await repository.persist(memo).unwrap();

      const found = await repository.find(memo.identifier).unwrap();

      expect(found.identifier).toBe(memo.identifier);
      expect(found.title).toBe(memo.title);
      expect(found.slug).toBe(memo.slug);
      expect(found.entries.length).toBe(memo.entries.length);
      expect(found.tags).toEqual(memo.tags);
      expect(found.status).toBe(memo.status);
      expect(found.timeline.createdAt.getTime()).toBe(
        memo.timeline.createdAt.getTime()
      );
      expect(found.timeline.updatedAt.getTime()).toBe(
        memo.timeline.updatedAt.getTime()
      );
    });
  });
});
