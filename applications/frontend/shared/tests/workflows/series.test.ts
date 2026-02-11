import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  createSeriesFindWorkflow,
  createSeriesFindBySlugWorkflow,
  createSeriesSearchWorkflow,
  createSeriesPersistWorkflow,
  createSeriesTerminateWorkflow,
} from "@shared/workflows/series";
import {
  validateSeriesIdentifier,
  validateSeries,
  validateCriteria,
} from "@shared/domains/series";
import { validateSlug } from "@shared/domains/common/slug";
import { Logger, Environment } from "@shared/aspects/logger";
import { ok, err } from "@shared/aspects/result";
import { aggregateNotFoundError, unexpectedError } from "@shared/aspects/error";
import {
  SeriesMold,
  SeriesIdentifierMold,
  SeriesSlugMold,
} from "../support/molds/domains/series";
import { Command } from "@shared/workflows/common";

describe("workflows/series", () => {
  const mockLogger = Logger(Environment.DEVELOPMENT);

  beforeEach(() => {
    vi.spyOn(console, "log").mockImplementation(() => {});
  });

  describe("createSeriesFindWorkflow", () => {
    it("有効なidentifierでシリーズを取得できる", async () => {
      const series = Forger(SeriesMold).forgeWithSeed(1);
      const findMock = vi.fn().mockReturnValue(ok(series).toAsync());

      const workflow = createSeriesFindWorkflow(validateSeriesIdentifier)(
        findMock
      )(mockLogger);

      const result = await workflow(series.identifier).unwrap();

      expect(result).toEqual(series);
      expect(findMock).toHaveBeenCalledWith(series.identifier);
    });

    it("無効なidentifierでValidationErrorを返す", async () => {
      const findMock = vi.fn();

      const workflow = createSeriesFindWorkflow(validateSeriesIdentifier)(
        findMock
      )(mockLogger);

      const result = workflow("invalid-identifier");

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(findMock).not.toHaveBeenCalled();
    });

    it("シリーズが見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const identifier = Forger(SeriesIdentifierMold).forgeWithSeed(1);
      const notFoundError = aggregateNotFoundError(
        "Series",
        "Series not found"
      );
      const findMock = vi.fn().mockReturnValue(err(notFoundError).toAsync());

      const workflow = createSeriesFindWorkflow(validateSeriesIdentifier)(
        findMock
      )(mockLogger);

      const result = await workflow(identifier).unwrapError();

      expect(result).toEqual(notFoundError);
    });
  });

  describe("createSeriesFindBySlugWorkflow", () => {
    it("有効なslugでシリーズを取得できる", async () => {
      const series = Forger(SeriesMold).forgeWithSeed(1);
      const findBySlugMock = vi.fn().mockReturnValue(ok(series).toAsync());

      const workflow = createSeriesFindBySlugWorkflow(validateSlug)(mockLogger)(
        findBySlugMock
      );

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug: series.slug },
      };

      const result = await workflow(command).unwrap();

      expect(result).toEqual(series);
      expect(findBySlugMock).toHaveBeenCalledWith(series.slug);
    });

    it("無効なslugでValidationErrorを返す", async () => {
      const findBySlugMock = vi.fn();

      const workflow = createSeriesFindBySlugWorkflow(validateSlug)(mockLogger)(
        findBySlugMock
      );

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug: "Invalid Slug With Spaces" },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(findBySlugMock).not.toHaveBeenCalled();
    });

    it("シリーズが見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const slug = Forger(SeriesSlugMold).forgeWithSeed(1);
      const notFoundError = aggregateNotFoundError(
        "Series",
        "Series not found"
      );
      const findBySlugMock = vi
        .fn()
        .mockReturnValue(err(notFoundError).toAsync());

      const workflow = createSeriesFindBySlugWorkflow(validateSlug)(mockLogger)(
        findBySlugMock
      );

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(notFoundError);
    });
  });

  describe("createSeriesSearchWorkflow", () => {
    it("有効な検索条件でシリーズを検索できる", async () => {
      const seriesList = Forger(SeriesMold).forgeMultiWithSeed(3, 1);
      const searchMock = vi.fn().mockReturnValue(ok(seriesList).toAsync());

      const workflow = createSeriesSearchWorkflow(validateCriteria)(searchMock)(
        mockLogger
      );

      const result = await workflow({ slug: null, tags: null }).unwrap();

      expect(result).toEqual(seriesList);
      expect(searchMock).toHaveBeenCalled();
    });

    it("slugを指定して検索できる", async () => {
      const seriesList = Forger(SeriesMold).forgeMultiWithSeed(1, 1);
      const slug = seriesList[0].slug;
      const searchMock = vi.fn().mockReturnValue(ok(seriesList).toAsync());

      const workflow = createSeriesSearchWorkflow(validateCriteria)(searchMock)(
        mockLogger
      );

      const result = await workflow({ slug, tags: null }).unwrap();

      expect(result).toEqual(seriesList);
    });

    it("検索でエラーが発生した場合はUnexpectedErrorを返す", async () => {
      const error = unexpectedError("Search failed");
      const searchMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createSeriesSearchWorkflow(validateCriteria)(searchMock)(
        mockLogger
      );

      const result = await workflow({ slug: null, tags: null }).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createSeriesPersistWorkflow", () => {
    it("有効なシリーズデータでシリーズを作成できる", async () => {
      const series = Forger(SeriesMold).forgeWithSeed(1);
      const persistMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createSeriesPersistWorkflow(validateSeries)(persistMock)(
        mockLogger
      );

      const result = await workflow({
        identifier: series.identifier,
        title: series.title,
        slug: series.slug,
        tags: series.tags,
        subTitle: null,
        chapters: series.chapters,
        description: series.description,
        cover: series.cover,
        timeline: series.timeline,
      }).unwrap();

      expect(result.payload.series).toBe(series.identifier);
      expect(persistMock).toHaveBeenCalled();
    });

    it("無効なシリーズデータでValidationErrorを返す", async () => {
      const persistMock = vi.fn();

      const workflow = createSeriesPersistWorkflow(validateSeries)(persistMock)(
        mockLogger
      );

      const result = workflow({
        identifier: "invalid",
        title: "", // 空のタイトルは無効
        slug: "test-slug",
        tags: [],
        subTitle: null,
        chapters: [],
        timeline: { createdAt: new Date(), updatedAt: new Date() },
      });

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(persistMock).not.toHaveBeenCalled();
    });

    it("永続化でエラーが発生した場合はエラーを返す", async () => {
      const series = Forger(SeriesMold).forgeWithSeed(1);
      const error = unexpectedError("Persist failed");
      const persistMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createSeriesPersistWorkflow(validateSeries)(persistMock)(
        mockLogger
      );

      const result = await workflow({
        identifier: series.identifier,
        title: series.title,
        slug: series.slug,
        tags: series.tags,
        subTitle: null,
        chapters: series.chapters,
        description: series.description,
        cover: series.cover,
        timeline: series.timeline,
      }).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createSeriesTerminateWorkflow", () => {
    it("有効なidentifierでシリーズを削除できる", async () => {
      const identifier = Forger(SeriesIdentifierMold).forgeWithSeed(1);
      const terminateMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createSeriesTerminateWorkflow(validateSeriesIdentifier)(
        terminateMock
      )(mockLogger);

      const result = await workflow(identifier).unwrap();

      expect(result.payload.series).toBe(identifier);
      expect(terminateMock).toHaveBeenCalledWith(identifier);
    });

    it("無効なidentifierでValidationErrorを返す", async () => {
      const terminateMock = vi.fn();

      const workflow = createSeriesTerminateWorkflow(validateSeriesIdentifier)(
        terminateMock
      )(mockLogger);

      const result = workflow("invalid-identifier");

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(terminateMock).not.toHaveBeenCalled();
    });

    it("シリーズが見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const identifier = Forger(SeriesIdentifierMold).forgeWithSeed(1);
      const notFoundError = aggregateNotFoundError(
        "Series",
        "Series not found"
      );
      const terminateMock = vi
        .fn()
        .mockReturnValue(err(notFoundError).toAsync());

      const workflow = createSeriesTerminateWorkflow(validateSeriesIdentifier)(
        terminateMock
      )(mockLogger);

      const result = await workflow(identifier).unwrapError();

      expect(result).toEqual(notFoundError);
    });
  });
});
