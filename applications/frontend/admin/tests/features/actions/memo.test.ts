/**
 * Memo Actions Feature Test
 *
 * Firebase Emulatorを使用してメモアクションの統合テストを行います。
 * Server Actionは`revalidateTag`などNext.js固有機能を使用するため、
 * ワークフローのコアロジックを直接テストします。
 */

import { describe, it, expect, beforeEach, afterAll } from "vitest";
import {
  createAdminFeatureTestContext,
  cleanupAdminFeatureTest,
  testLogger,
} from "../setup";
import {
  Memo,
  MemoSnapshot,
  toSnapshot,
  UnvalidatedCriteria,
  UnvalidatedEntry,
  UnvalidatedMemo,
  validateCriteria,
  validateEntry,
  validateMemo,
  validateMemoIdentifier,
} from "@shared/domains/memo";
import {
  PublishStatus,
  validateSlug,
  slugSchema,
} from "@shared/domains/common";
import {
  createMemoCreateWorkflow,
  createMemoEditWorkflow,
  createMemoFindWorkflow,
  createMemoSearchWorkflow,
  createMemoTerminateWorkflow,
  createPersistMemoEntryWorkflow,
} from "@shared/workflows/memo";
import {
  MemoMold,
  MemoEntryMold,
  MemoProperties,
  MemoEntryProperties,
} from "@shared-tests/support/molds/domains/memo";
import {
  isValidationError,
  isAggregateNotFoundError,
} from "@shared/aspects/error";
import { FirebaseMemoRepository } from "@shared/infrastructures/memos";
import {
  getTestFirestoreInstance,
  createFirestoreOperations,
  cleanupTestApp,
} from "../firebase-test-utils";
import type { Firestore } from "firebase/firestore";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import { Forger } from "@lihs-ie/forger-ts";

const TEST_APP_NAME = "memo-feature-test-app";

type SuccessResult<T> = { success: true; data: T };
type FailureResult<E> = { success: false; error: E };
type ResultOutcome<T, E> = SuccessResult<T> | FailureResult<E>;

function memoToUnvalidated(memo: Memo): UnvalidatedMemo {
  return {
    identifier: memo.identifier,
    title: memo.title,
    slug: memo.slug,
    entries: memo.entries.map((entry) => ({
      text: entry.text,
      createdAt: entry.createdAt,
    })),
    tags: [...memo.tags],
    status: memo.status,
    timeline: {
      createdAt: memo.timeline.createdAt,
      updatedAt: memo.timeline.updatedAt,
    },
  };
}

describe("Feature: Memo Actions (実DB接続)", () => {
  let firestore: Firestore;
  let operations: FirestoreOperations;
  let memoRepository: ReturnType<typeof FirebaseMemoRepository>;

  const buildMemo = (overrides?: Partial<MemoProperties>) =>
    Forger(MemoMold).forge(overrides);

  const buildMemoEntry = (overrides?: Partial<MemoEntryProperties>) =>
    Forger(MemoEntryMold).forge(overrides);

  beforeEach(async () => {
    await createAdminFeatureTestContext();
    firestore = getTestFirestoreInstance(TEST_APP_NAME);
    operations = createFirestoreOperations();
    memoRepository = FirebaseMemoRepository(firestore, operations);
  }, 30000);

  afterAll(async () => {
    await cleanupAdminFeatureTest();
    await cleanupTestApp(TEST_APP_NAME);
  }, 30000);

  describe("メモ作成ワークフロー", () => {
    it("メモを作成し永続化されることを確認", async () => {
      const memo = buildMemo({
        status: PublishStatus.PUBLISHED,
      });

      const createWorkflow = createMemoCreateWorkflow(validateMemo)(
        memoRepository.persist,
      )(testLogger);

      const createResult = await createWorkflow({
        payload: { unvalidated: memoToUnvalidated(memo) },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(createResult.success).toBe(true);

      const findWorkflow = createMemoFindWorkflow(validateMemoIdentifier)(
        memoRepository.find,
      )(testLogger);

      const findResult = await findWorkflow(memo.identifier).match<
        ResultOutcome<Memo, unknown>
      >({
        ok: (found) => ({ success: true, data: found }),
        err: (error) => ({ success: false, error }),
      });

      expect(findResult.success).toBe(true);
      if (findResult.success) {
        expect(findResult.data.identifier).toBe(memo.identifier);
        expect(findResult.data.title).toBe(memo.title);
        expect(findResult.data.slug).toBe(memo.slug);
        expect(findResult.data.status).toBe(memo.status);
      }
    });
  });

  describe("メモエントリ追加ワークフロー", () => {
    it("メモにエントリを追加できることを確認", async () => {
      const memo = buildMemo({
        status: PublishStatus.PUBLISHED,
        entries: [],
      });

      const createWorkflow = createMemoCreateWorkflow(validateMemo)(
        memoRepository.persist,
      )(testLogger);

      await createWorkflow({
        payload: { unvalidated: memoToUnvalidated(memo) },
        now: new Date(),
      }).unwrap();

      const newEntry = buildMemoEntry();

      const unvalidatedEntry: UnvalidatedEntry = {
        text: newEntry.text,
        createdAt: newEntry.createdAt,
      };

      const addEntryWorkflow = createPersistMemoEntryWorkflow(validateSlug)(
        validateEntry,
      )(memoRepository.findBySlug)(memoRepository.persist)(testLogger);

      const addEntryResult = await addEntryWorkflow({
        payload: { slug: memo.slug, unvalidated: unvalidatedEntry },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(addEntryResult.success).toBe(true);

      const findWorkflow = createMemoFindWorkflow(validateMemoIdentifier)(
        memoRepository.find,
      )(testLogger);

      const findResult = await findWorkflow(memo.identifier).match<
        ResultOutcome<Memo, unknown>
      >({
        ok: (found) => ({ success: true, data: found }),
        err: (error) => ({ success: false, error }),
      });

      expect(findResult.success).toBe(true);
      if (findResult.success) {
        expect(findResult.data.entries.length).toBe(1);
        expect(findResult.data.entries[0].text).toBe(newEntry.text);
      }
    });
  });

  describe("メモ編集ワークフロー", () => {
    it("メモを編集し変更が反映されることを確認", async () => {
      const memo = buildMemo({
        status: PublishStatus.DRAFT,
      });
      const unvalidatedMemo = memoToUnvalidated(memo);

      const createWorkflow = createMemoCreateWorkflow(validateMemo)(
        memoRepository.persist,
      )(testLogger);

      await createWorkflow({
        payload: { unvalidated: unvalidatedMemo },
        now: new Date(),
      }).unwrap();

      const findWorkflow = createMemoFindWorkflow(validateMemoIdentifier)(
        memoRepository.find,
      )(testLogger);

      const findBeforeEditResult = await findWorkflow(memo.identifier).match<
        ResultOutcome<Memo, unknown>
      >({
        ok: (found) => ({ success: true, data: found }),
        err: (error) => ({ success: false, error }),
      });

      expect(findBeforeEditResult.success).toBe(true);

      const originalSnapshot: MemoSnapshot = toSnapshot(memo);
      const updatedTitle = "Updated Memo Title - " + Date.now();
      const updatedAt = new Date(memo.timeline.createdAt);
      updatedAt.setDate(updatedAt.getDate() + 10);

      const updatedUnvalidatedMemo: UnvalidatedMemo = {
        ...unvalidatedMemo,
        title: updatedTitle,
        status: PublishStatus.PUBLISHED,
        timeline: {
          createdAt: memo.timeline.createdAt,
          updatedAt: updatedAt,
        },
      };

      const editWorkflow = createMemoEditWorkflow(validateMemo)(
        memoRepository.persist,
      )(testLogger);

      const editResult = await editWorkflow({
        payload: {
          unvalidated: updatedUnvalidatedMemo,
          before: originalSnapshot,
        },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(editResult.success).toBe(true);

      const findAfterEditResult = await findWorkflow(memo.identifier).match<
        ResultOutcome<Memo, unknown>
      >({
        ok: (found) => ({ success: true, data: found }),
        err: (error) => ({ success: false, error }),
      });

      expect(findAfterEditResult.success).toBe(true);
      if (findAfterEditResult.success) {
        expect(findAfterEditResult.data.title).toBe(updatedTitle);
        expect(findAfterEditResult.data.status).toBe(PublishStatus.PUBLISHED);
      }
    });
  });

  describe("メモ検索ワークフロー", () => {
    it("メモを検索条件で取得できることを確認", async () => {
      const uniqueSlug = slugSchema.parse(`test-memo-slug-${Date.now()}`);
      const memo = buildMemo({
        slug: uniqueSlug,
        status: PublishStatus.PUBLISHED,
      });

      const createWorkflow = createMemoCreateWorkflow(validateMemo)(
        memoRepository.persist,
      )(testLogger);

      await createWorkflow({
        payload: { unvalidated: memoToUnvalidated(memo) },
        now: new Date(),
      }).unwrap();

      const searchWorkflow = createMemoSearchWorkflow(validateCriteria)(
        memoRepository.search,
      )(testLogger);

      const criteria: UnvalidatedCriteria = {
        tags: null,
        freeWord: null,
        status: null,
      };

      const searchResult = await searchWorkflow({
        payload: criteria,
        now: new Date(),
      }).match<ResultOutcome<Memo[], unknown>>({
        ok: (memos) => ({ success: true, data: memos }),
        err: (error) => ({ success: false, error }),
      });

      expect(searchResult.success).toBe(true);
      if (searchResult.success) {
        expect(searchResult.data.length).toBeGreaterThan(0);
        const found = searchResult.data.find(
          (memoItem) => memoItem.identifier === memo.identifier,
        );
        expect(found).toBeDefined();
        expect(found?.slug).toBe(uniqueSlug);
      }
    });
  });

  describe("メモ削除ワークフロー", () => {
    it("メモを削除し取得できなくなることを確認", async () => {
      const memo = buildMemo();

      const createWorkflow = createMemoCreateWorkflow(validateMemo)(
        memoRepository.persist,
      )(testLogger);

      await createWorkflow({
        payload: { unvalidated: memoToUnvalidated(memo) },
        now: new Date(),
      }).unwrap();

      const terminateWorkflow = createMemoTerminateWorkflow(
        validateMemoIdentifier,
      )(memoRepository.terminate)(testLogger);

      const terminateResult = await terminateWorkflow({
        payload: { identifier: memo.identifier },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(terminateResult.success).toBe(true);

      const findWorkflow = createMemoFindWorkflow(validateMemoIdentifier)(
        memoRepository.find,
      )(testLogger);

      const findResult = await findWorkflow(memo.identifier).match<
        ResultOutcome<Memo, unknown>
      >({
        ok: (found) => ({ success: true, data: found }),
        err: (error) => ({ success: false, error }),
      });

      expect(findResult.success).toBe(false);
      if (!findResult.success) {
        expect(isAggregateNotFoundError(findResult.error)).toBe(true);
      }
    });
  });

  describe("バリデーションエラー", () => {
    it("無効なデータでValidationErrorが返る", async () => {
      const invalidUnvalidatedMemo: UnvalidatedMemo = {
        identifier: "invalid-ulid",
        title: "",
        slug: "valid-slug",
        entries: [],
        tags: [],
        status: "published",
        timeline: {
          createdAt: new Date(),
          updatedAt: new Date(),
        },
      };

      const createWorkflow = createMemoCreateWorkflow(validateMemo)(
        memoRepository.persist,
      )(testLogger);

      const result = await createWorkflow({
        payload: { unvalidated: invalidUnvalidatedMemo },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (errors) => ({ success: false, errors }),
      });

      expect(result.success).toBe(false);
      if (!result.success && "errors" in result) {
        expect(Array.isArray(result.errors)).toBe(true);
        if (Array.isArray(result.errors)) {
          expect(result.errors.length).toBeGreaterThan(0);
          expect(result.errors.every((error) => isValidationError(error))).toBe(
            true,
          );
        }
      }
    });

    it("空のタイトルでValidationErrorが返る", async () => {
      const memo = buildMemo();
      const unvalidated = memoToUnvalidated(memo);

      const createWorkflow = createMemoCreateWorkflow(validateMemo)(
        memoRepository.persist,
      )(testLogger);

      const result = await createWorkflow({
        payload: { unvalidated: { ...unvalidated, title: "" } },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (errors) => ({ success: false, errors }),
      });

      expect(result.success).toBe(false);
      if (!result.success && "errors" in result) {
        expect(Array.isArray(result.errors)).toBe(true);
      }
    });

    it("空のエントリテキストでValidationErrorが返る", async () => {
      const memo = buildMemo({
        status: PublishStatus.PUBLISHED,
        entries: [],
      });

      const createWorkflow = createMemoCreateWorkflow(validateMemo)(
        memoRepository.persist,
      )(testLogger);

      await createWorkflow({
        payload: { unvalidated: memoToUnvalidated(memo) },
        now: new Date(),
      }).unwrap();

      const invalidEntry: UnvalidatedEntry = {
        text: "",
        createdAt: new Date(),
      };

      const addEntryWorkflow = createPersistMemoEntryWorkflow(validateSlug)(
        validateEntry,
      )(memoRepository.findBySlug)(memoRepository.persist)(testLogger);

      const result = await addEntryWorkflow({
        payload: { slug: memo.slug, unvalidated: invalidEntry },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (errors) => ({ success: false, errors }),
      });

      expect(result.success).toBe(false);
      if (!result.success && "errors" in result) {
        expect(Array.isArray(result.errors)).toBe(true);
      }
    });
  });

  describe("重複エラー", () => {
    it("同じslugのメモを作成するとエラーが返る", async () => {
      const sharedSlug = slugSchema.parse(`duplicate-slug-${Date.now()}`);

      const memo1 = buildMemo({ slug: sharedSlug });
      const memo2 = buildMemo({ slug: sharedSlug });

      const createWorkflow = createMemoCreateWorkflow(validateMemo)(
        memoRepository.persist,
      )(testLogger);

      const result1 = await createWorkflow({
        payload: { unvalidated: memoToUnvalidated(memo1) },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(result1.success).toBe(true);

      const result2 = await createWorkflow({
        payload: { unvalidated: memoToUnvalidated(memo2) },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(result2.success).toBe(false);
    });
  });
});
