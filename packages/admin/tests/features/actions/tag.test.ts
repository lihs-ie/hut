/**
 * Tag Actions Feature Test
 *
 * Firebase Emulatorを使用してタグアクションの統合テストを行います。
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
  Tag,
  UnvalidatedTag,
  validateTag,
  validateTagIdentifier,
} from "@shared/domains/attributes/tag";
import {
  createTagPersistWorkflow,
  createTagTerminateWorkflow,
  createTagFindWorkflow,
} from "@shared/workflows/attributes/tag";
import {
  TagMold,
  TagProperties,
} from "@shared-tests/support/molds/domains/attributes/tag";
import {
  isValidationError,
  isAggregateNotFoundError,
} from "@shared/aspects/error";
import { FirebaseTagRepository } from "@shared/infrastructures/tags";
import {
  getTestFirestoreInstance,
  createFirestoreOperations,
  cleanupTestApp,
} from "../firebase-test-utils";
import type { Firestore } from "firebase/firestore";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import { Forger } from "@lihs-ie/forger-ts";

const TEST_APP_NAME = "tag-feature-test-app";

type SuccessResult<T> = { success: true; data: T };
type FailureResult<E> = { success: false; error: E };
type ResultOutcome<T, E> = SuccessResult<T> | FailureResult<E>;

function tagToUnvalidated(tag: Tag): UnvalidatedTag {
  return {
    identifier: tag.identifier,
    name: tag.name,
    logo: tag.logo,
    timeline: {
      createdAt: tag.timeline.createdAt,
      updatedAt: tag.timeline.updatedAt,
    },
  };
}

describe("Feature: Tag Actions (実DB接続)", () => {
  let firestore: Firestore;
  let operations: FirestoreOperations;
  let tagRepository: ReturnType<typeof FirebaseTagRepository>;

  const buildTag = (overrides?: Partial<TagProperties>) =>
    Forger(TagMold).forge(overrides);

  beforeEach(async () => {
    await createAdminFeatureTestContext();
    firestore = getTestFirestoreInstance(TEST_APP_NAME);
    operations = createFirestoreOperations();
    tagRepository = FirebaseTagRepository(firestore, operations);
  }, 30000);

  afterAll(async () => {
    await cleanupAdminFeatureTest();
    await cleanupTestApp(TEST_APP_NAME);
  }, 30000);

  describe("タグ永続化ワークフロー", () => {
    it("タグを永続化できることを確認", async () => {
      const tag = buildTag();

      const persistWorkflow = createTagPersistWorkflow(validateTag)(
        tagRepository.persist,
      )(testLogger);

      const persistResult = await persistWorkflow({
        payload: tagToUnvalidated(tag),
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(persistResult.success).toBe(true);

      const findWorkflow = createTagFindWorkflow(validateTagIdentifier)(
        tagRepository.find,
      )(testLogger);

      const findResult = await findWorkflow({
        payload: tag.identifier,
        now: new Date(),
      }).match<ResultOutcome<Tag, unknown>>({
        ok: (found) => ({ success: true, data: found }),
        err: (error) => ({ success: false, error }),
      });

      expect(findResult.success).toBe(true);
      if (findResult.success) {
        expect(findResult.data.identifier).toBe(tag.identifier);
        expect(findResult.data.name).toBe(tag.name);
        expect(findResult.data.logo).toBe(tag.logo);
      }
    });
  });

  describe("タグ削除ワークフロー", () => {
    it("タグを削除し取得できなくなることを確認", async () => {
      const tag = buildTag();

      const persistWorkflow = createTagPersistWorkflow(validateTag)(
        tagRepository.persist,
      )(testLogger);

      await persistWorkflow({
        payload: tagToUnvalidated(tag),
        now: new Date(),
      }).unwrap();

      const terminateWorkflow = createTagTerminateWorkflow(
        validateTagIdentifier,
      )(tagRepository.terminate)(testLogger);

      const terminateResult = await terminateWorkflow({
        payload: tag.identifier,
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(terminateResult.success).toBe(true);

      const findWorkflow = createTagFindWorkflow(validateTagIdentifier)(
        tagRepository.find,
      )(testLogger);

      const findResult = await findWorkflow({
        payload: tag.identifier,
        now: new Date(),
      }).match<ResultOutcome<Tag, unknown>>({
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
      const invalidUnvalidatedTag: UnvalidatedTag = {
        identifier: "invalid-ulid",
        name: "",
        logo: "",
        timeline: {
          createdAt: new Date(),
          updatedAt: new Date(),
        },
      };

      const persistWorkflow = createTagPersistWorkflow(validateTag)(
        tagRepository.persist,
      )(testLogger);

      const result = await persistWorkflow({
        payload: invalidUnvalidatedTag,
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

    it("空の名前でValidationErrorが返る", async () => {
      const tag = buildTag();
      const unvalidated = tagToUnvalidated(tag);

      const persistWorkflow = createTagPersistWorkflow(validateTag)(
        tagRepository.persist,
      )(testLogger);

      const result = await persistWorkflow({
        payload: { ...unvalidated, name: "" },
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

    it("長すぎる名前でValidationErrorが返る", async () => {
      const tag = buildTag();
      const unvalidated = tagToUnvalidated(tag);

      const persistWorkflow = createTagPersistWorkflow(validateTag)(
        tagRepository.persist,
      )(testLogger);

      const result = await persistWorkflow({
        payload: { ...unvalidated, name: "a".repeat(21) },
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
    it("同じ名前のタグを作成するとエラーが返る", async () => {
      const sharedName = `dup-${Date.now() % 100000}`;

      const tag1 = buildTag();
      const tag2 = buildTag();

      const persistWorkflow = createTagPersistWorkflow(validateTag)(
        tagRepository.persist,
      )(testLogger);

      const result1 = await persistWorkflow({
        payload: { ...tagToUnvalidated(tag1), name: sharedName },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(result1.success).toBe(true);

      const result2 = await persistWorkflow({
        payload: { ...tagToUnvalidated(tag2), name: sharedName },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(result2.success).toBe(false);
    });
  });
});
