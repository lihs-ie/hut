/**
 * Series Actions Feature Test
 *
 * Firebase Emulatorを使用してシリーズアクションの統合テストを行います。
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
  Series,
  UnvalidatedSeries,
  validateSeries,
  validateSeriesIdentifier,
} from "@shared/domains/series";
import {
  createSeriesPersistWorkflow,
  createSeriesTerminateWorkflow,
  createSeriesFindWorkflow,
} from "@shared/workflows/series";
import {
  SeriesMold,
  type SeriesProperties,
} from "@shared-tests/support/molds/domains/series/common";
import {
  isValidationError,
  isAggregateNotFoundError,
} from "@shared/aspects/error";
import { FirebaseSeriesRepository } from "@shared/infrastructures/series";
import {
  getTestFirestoreInstance,
  createFirestoreOperations,
  cleanupTestApp,
} from "../firebase-test-utils";
import type { Firestore } from "firebase/firestore";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import { Forger } from "@lihs-ie/forger-ts";

type SuccessResult<T> = { success: true; data: T };
type FailureResult<E> = { success: false; error: E };
type ResultOutcome<T, E> = SuccessResult<T> | FailureResult<E>;

const TEST_APP_NAME = "series-feature-test-app";

const buildSeries = (overrides?: Partial<SeriesProperties>) =>
  Forger(SeriesMold).forge(overrides);

function seriesToUnvalidated(series: Series): UnvalidatedSeries {
  return {
    identifier: series.identifier,
    title: series.title,
    slug: series.slug,
    tags: [...series.tags],
    subTitle: series.subTitle ?? null,
    description: series.description,
    cover: series.cover,
    chapters: series.chapters.map((chapter) => ({
      title: chapter.title,
      slug: chapter.slug,
      content: chapter.content,
      timeline: {
        createdAt: chapter.timeline.createdAt,
        updatedAt: chapter.timeline.updatedAt,
      },
    })),
    timeline: {
      createdAt: series.timeline.createdAt,
      updatedAt: series.timeline.updatedAt,
    },
  };
}

describe("Feature: Series Actions (実DB接続)", () => {
  let firestore: Firestore;
  let operations: FirestoreOperations;
  let seriesRepository: ReturnType<typeof FirebaseSeriesRepository>;

  beforeEach(async () => {
    await createAdminFeatureTestContext();
    firestore = getTestFirestoreInstance(TEST_APP_NAME);
    operations = createFirestoreOperations();
    seriesRepository = FirebaseSeriesRepository(firestore, operations);
  }, 30000);

  afterAll(async () => {
    await cleanupAdminFeatureTest();
    await cleanupTestApp(TEST_APP_NAME);
  }, 30000);

  describe("シリーズ永続化ワークフロー", () => {
    it("シリーズを永続化できることを確認", async () => {
      const series = buildSeries();

      const persistWorkflow = createSeriesPersistWorkflow(validateSeries)(
        seriesRepository.persist,
      )(testLogger);

      const persistResult = await persistWorkflow(
        seriesToUnvalidated(series),
      ).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(persistResult.success).toBe(true);

      const findWorkflow = createSeriesFindWorkflow(validateSeriesIdentifier)(
        seriesRepository.find,
      )(testLogger);

      const findResult = await findWorkflow(series.identifier).match<
        ResultOutcome<Series, unknown>
      >({
        ok: (found) => ({ success: true, data: found }),
        err: (error) => ({ success: false, error }),
      });

      expect(findResult.success).toBe(true);
      if (findResult.success && "data" in findResult) {
        const found = findResult.data;
        expect(found.identifier).toBe(series.identifier);
        expect(found.title).toBe(series.title);
        expect(found.slug).toBe(series.slug);
        expect(found.chapters.length).toBe(series.chapters.length);
      }
    });
  });

  describe("シリーズ削除ワークフロー", () => {
    it("シリーズを削除し取得できなくなることを確認", async () => {
      const series = buildSeries();

      const persistWorkflow = createSeriesPersistWorkflow(validateSeries)(
        seriesRepository.persist,
      )(testLogger);

      await persistWorkflow(seriesToUnvalidated(series)).unwrap();

      const terminateWorkflow = createSeriesTerminateWorkflow(
        validateSeriesIdentifier,
      )(seriesRepository.terminate)(testLogger);

      const terminateResult = await terminateWorkflow(series.identifier).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(terminateResult.success).toBe(true);

      const findWorkflow = createSeriesFindWorkflow(validateSeriesIdentifier)(
        seriesRepository.find,
      )(testLogger);

      const findResult = await findWorkflow(series.identifier).match<
        ResultOutcome<Series, unknown>
      >({
        ok: (found) => ({ success: true, data: found }),
        err: (error) => ({ success: false, error }),
      });

      expect(findResult.success).toBe(false);
      if (!findResult.success && "error" in findResult) {
        expect(isAggregateNotFoundError(findResult.error)).toBe(true);
      }
    });
  });

  describe("バリデーションエラー", () => {
    it("無効なデータでValidationErrorが返る", async () => {
      const invalidUnvalidatedSeries: UnvalidatedSeries = {
        identifier: "invalid-ulid",
        title: "",
        slug: "",
        tags: [],
        subTitle: null,
        description: undefined,
        cover: null,
        chapters: [],
        timeline: {
          createdAt: new Date(),
          updatedAt: new Date(),
        },
      };

      const persistWorkflow = createSeriesPersistWorkflow(validateSeries)(
        seriesRepository.persist,
      )(testLogger);

      const result = await persistWorkflow(invalidUnvalidatedSeries).match({
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
      const series = buildSeries();
      const unvalidated = seriesToUnvalidated(series);

      const persistWorkflow = createSeriesPersistWorkflow(validateSeries)(
        seriesRepository.persist,
      )(testLogger);

      const result = await persistWorkflow({ ...unvalidated, title: "" }).match(
        {
          ok: () => ({ success: true }),
          err: (errors) => ({ success: false, errors }),
        },
      );

      expect(result.success).toBe(false);
      if (!result.success && "errors" in result) {
        expect(Array.isArray(result.errors)).toBe(true);
      }
    });

    it("無効なidentifierでValidationErrorが返る", async () => {
      const series = buildSeries();
      const unvalidated = seriesToUnvalidated(series);

      const persistWorkflow = createSeriesPersistWorkflow(validateSeries)(
        seriesRepository.persist,
      )(testLogger);

      const result = await persistWorkflow({
        ...unvalidated,
        identifier: "not-a-valid-ulid-format",
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
});
