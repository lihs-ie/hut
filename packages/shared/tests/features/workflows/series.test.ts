/**
 * Series Workflow Feature Test
 *
 * Firebase Emulatorを使用してSeriesワークフローの統合テストを行います。
 */

import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  createSeriesFindWorkflow,
  createSeriesFindBySlugWorkflow,
  createSeriesSearchWorkflow,
  createSeriesPersistWorkflow,
  createSeriesTerminateWorkflow,
} from "@shared/workflows/series";
import {
  validateSeries,
  validateSeriesIdentifier,
  validateCriteria,
} from "@shared/domains/series";
import { validateSlug } from "@shared/domains/common/slug";
import { FirebaseSeriesRepository } from "@shared/infrastructures/series";
import {
  createFeatureTestContext,
  cleanupFeatureTest,
  testLogger,
  type FeatureTestContext,
} from "../setup";
import {
  SeriesMold,
  SeriesIdentifierMold,
  SeriesSlugMold,
} from "../../support/molds/domains/series";
import type {
  Series,
  SeriesRepository,
  SeriesCreatedEvent,
} from "@shared/domains/series";

function toSeriesPayload(series: Series) {
  return {
    identifier: series.identifier,
    title: series.title,
    slug: series.slug,
    tags: series.tags,
    subTitle: series.subTitle ?? null,
    description: series.description,
    cover: series.cover,
    chapters: series.chapters,
    timeline: series.timeline,
  };
}

describe("Feature: Series Workflow (実DB接続)", () => {
  let context: FeatureTestContext;
  let repository: SeriesRepository;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});

    context = await createFeatureTestContext();
    repository = FirebaseSeriesRepository(
      context.firestore,
      context.operations
    );
  }, 30000);

  afterAll(async () => {
    await cleanupFeatureTest();
  }, 30000);

  describe("シリーズ作成から検索までの一連の流れ", () => {
    it("新規シリーズを作成し、そのシリーズを検索して取得できる", async () => {
      const persistWorkflow = createSeriesPersistWorkflow(validateSeries)(
        repository.persist
      )(testLogger);
      const searchWorkflow = createSeriesSearchWorkflow(validateCriteria)(
        repository.search
      )(testLogger);

      const series = Forger(SeriesMold).forgeWithSeed(1);

      const persistResult = await persistWorkflow(
        toSeriesPayload(series)
      ).unwrap();

      expect(persistResult.payload.series).toBe(series.identifier);
      expect(persistResult.occurredAt).toBeDefined();

      const searchResult = await searchWorkflow({
        slug: null,
        tags: null,
      }).unwrap();

      expect(searchResult.length).toBeGreaterThanOrEqual(1);
      expect(
        searchResult.some((item) => item.identifier === series.identifier)
      ).toBe(true);
    });

    it("シリーズを作成後、identifierで取得できる", async () => {
      const persistWorkflow = createSeriesPersistWorkflow(validateSeries)(
        repository.persist
      )(testLogger);
      const findWorkflow = createSeriesFindWorkflow(validateSeriesIdentifier)(
        repository.find
      )(testLogger);

      const series = Forger(SeriesMold).forgeWithSeed(2);

      await persistWorkflow(toSeriesPayload(series)).unwrap();

      const result = await findWorkflow(series.identifier).unwrap();

      expect(result.identifier).toBe(series.identifier);
      expect(result.title).toBe(series.title);
    });
  });

  describe("シリーズ検索ワークフロー", () => {
    it("identifierでシリーズを取得できる", async () => {
      const series = Forger(SeriesMold).forgeWithSeed(10);

      await repository.persist(series).unwrap();

      const findWorkflow = createSeriesFindWorkflow(validateSeriesIdentifier)(
        repository.find
      )(testLogger);

      const result = await findWorkflow(series.identifier).unwrap();

      expect(result.identifier).toBe(series.identifier);
      expect(result.title).toBe(series.title);
    });

    it("slugでシリーズを取得できる", async () => {
      const slug = Forger(SeriesSlugMold).forgeWithSeed(20, {
        value: "feature-test-series-slug",
      });
      const series = Forger(SeriesMold).forgeWithSeed(21, { slug });

      await repository.persist(series).unwrap();

      const findBySlugWorkflow = createSeriesFindBySlugWorkflow(validateSlug)(
        testLogger
      )(repository.findBySlug);

      const result = await findBySlugWorkflow({
        now: new Date(),
        payload: { slug },
      }).unwrap();

      expect(result.identifier).toBe(series.identifier);
      expect(result.slug).toBe(slug);
    });

    it("全件を取得して結果をフィルタリングできる", async () => {
      const series1 = Forger(SeriesMold).forgeWithSeed(30);
      const series2 = Forger(SeriesMold).forgeWithSeed(31);

      await repository.persist(series1).unwrap();
      await repository.persist(series2).unwrap();

      const searchWorkflow = createSeriesSearchWorkflow(validateCriteria)(
        repository.search
      )(testLogger);

      // slugとtagsがnullの場合は全件取得
      const result = await searchWorkflow({
        slug: null,
        tags: null,
      }).unwrap();

      // 両方のシリーズが取得できること
      expect(result.length).toBeGreaterThanOrEqual(2);
      expect(
        result.some((series) => series.identifier === series1.identifier)
      ).toBe(true);
      expect(
        result.some((series) => series.identifier === series2.identifier)
      ).toBe(true);
    });
  });

  describe("シリーズ削除ワークフロー", () => {
    it("シリーズを削除でき、その後検索で見つからない", async () => {
      const series = Forger(SeriesMold).forgeWithSeed(40);

      await repository.persist(series).unwrap();

      const terminateWorkflow = createSeriesTerminateWorkflow(
        validateSeriesIdentifier
      )(repository.terminate)(testLogger);
      const findWorkflow = createSeriesFindWorkflow(validateSeriesIdentifier)(
        repository.find
      )(testLogger);

      const terminateResult = await terminateWorkflow(
        series.identifier
      ).unwrap();

      expect(terminateResult.payload.series).toBe(series.identifier);
      expect(terminateResult.occurredAt).toBeDefined();

      const findResult = await findWorkflow(series.identifier).match({
        ok: () => "found",
        err: () => "not-found",
      });

      expect(findResult).toBe("not-found");
    });
  });

  describe("並列処理の最適化", () => {
    it("複数シリーズの作成が並列で実行できる", async () => {
      const persistWorkflow = createSeriesPersistWorkflow(validateSeries)(
        repository.persist
      )(testLogger);

      const seriesList = Forger(SeriesMold).forgeMultiWithSeed(3, 50);

      const results = await Promise.all(
        seriesList.map((series) =>
          persistWorkflow(toSeriesPayload(series)).unwrap()
        )
      );

      expect(results.length).toBe(3);
      results.forEach((result: SeriesCreatedEvent, index: number) => {
        expect(result.payload.series).toBe(seriesList[index]?.identifier);
      });

      // 永続化されたことを確認
      for (const series of seriesList) {
        const found = await repository.find(series.identifier).unwrap();
        expect(found.identifier).toBe(series.identifier);
      }
    });
  });

  describe("エラーハンドリング", () => {
    it("存在しないシリーズを検索するとAggregateNotFoundErrorが返る", async () => {
      const findWorkflow = createSeriesFindWorkflow(validateSeriesIdentifier)(
        repository.find
      )(testLogger);

      const nonExistentIdentifier =
        Forger(SeriesIdentifierMold).forgeWithSeed(100);

      const result = await findWorkflow(nonExistentIdentifier).match({
        ok: () => ({ found: true }),
        err: (error) => ({ found: false, error }),
      });

      expect(result.found).toBe(false);
    });

    it("無効なidentifierでValidationErrorが返る", async () => {
      const findWorkflow = createSeriesFindWorkflow(validateSeriesIdentifier)(
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
    it("作成したシリーズのデータが正確に永続化される", async () => {
      const series = Forger(SeriesMold).forgeWithSeed(110);

      await repository.persist(series).unwrap();

      const found = await repository.find(series.identifier).unwrap();

      expect(found.identifier).toBe(series.identifier);
      expect(found.title).toBe(series.title);
      expect(found.slug).toBe(series.slug);
      expect(found.tags).toEqual(series.tags);
      expect(found.description).toBe(series.description);
      expect(found.cover).toBe(series.cover);
      expect(found.chapters.length).toBe(series.chapters.length);
      expect(found.timeline.createdAt.getTime()).toBe(
        series.timeline.createdAt.getTime()
      );
      expect(found.timeline.updatedAt.getTime()).toBe(
        series.timeline.updatedAt.getTime()
      );
    });

    it("チャプターを含むシリーズが正確に永続化される", async () => {
      const series = Forger(SeriesMold).forgeWithSeed(120);

      await repository.persist(series).unwrap();

      const found = await repository.find(series.identifier).unwrap();

      expect(found.chapters.length).toBe(series.chapters.length);
      found.chapters.forEach((chapter, index) => {
        const originalChapter = series.chapters[index];
        expect(chapter.title).toBe(originalChapter?.title);
        expect(chapter.slug).toBe(originalChapter?.slug);
        expect(chapter.content).toBe(originalChapter?.content);
      });
    });
  });
});
