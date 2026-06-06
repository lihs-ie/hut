import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  createChapterFindBySlugWorkflow,
  createChapterPersistWorkflow,
  createChapterTerminateWorkflow,
  createChapterPersistWithSeriesWorkflow,
  createChapterTerminateWithSeriesWorkflow,
  ChapterPersistWorkflow,
  ChapterTerminateWorkflow,
} from "@shared/workflows/chapter";
import {
  createPassthroughFilter,
  createPublishedOnlyFilter,
} from "@shared/workflows/common";
import { validateSlug } from "@shared/domains/common/slug";
import { PublishStatus } from "@shared/domains/common";
import { isAggregateNotFoundError } from "@shared/aspects/error";
import {
  validateChapter,
  validateChapterIdentifier,
} from "@shared/domains/series/chapter";
import { Logger, Environment } from "@shared/aspects/logger";
import { ok, err } from "@shared/aspects/result";
import { aggregateNotFoundError } from "@shared/aspects/error";
import {
  ChapterMold,
  ChapterIdentifierMold,
} from "../support/molds/domains/series/chapter";
import { SlugMold } from "../support/molds/domains/common/slug";
import { SeriesMold } from "../support/molds/domains/series/common";
import { Command } from "@shared/workflows/common";

describe("workflows/chapter", () => {
  const mockLogger = Logger(Environment.DEVELOPMENT);

  beforeEach(() => {
    vi.spyOn(console, "log").mockImplementation(() => {});
  });

  describe("createChapterFindBySlugWorkflow", () => {
    it("有効なslugでChapterを取得できる", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(1);
      const findBySlugMock = vi.fn().mockReturnValue(ok(chapter).toAsync());

      const workflow = createChapterFindBySlugWorkflow(validateSlug)(
        mockLogger,
      )(findBySlugMock)(createPassthroughFilter());

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug: chapter.slug },
      };

      const result = await workflow(command).unwrap();

      expect(result).toEqual(chapter);
      expect(findBySlugMock).toHaveBeenCalledWith(chapter.slug);
    });

    it("無効なslugでValidationErrorを返す", async () => {
      const findBySlugMock = vi.fn();

      const workflow = createChapterFindBySlugWorkflow(validateSlug)(
        mockLogger,
      )(findBySlugMock)(createPassthroughFilter());

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug: "Invalid Slug With Spaces" },
      };

      const result = workflow(command);

      expect(
        await result.match({ ok: () => false, err: () => true }),
      ).toBe(true);
      expect(findBySlugMock).not.toHaveBeenCalled();
    });

    it("Chapterが見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const slug = Forger(SlugMold).forgeWithSeed(1);
      const notFoundError = aggregateNotFoundError(
        "Chapter",
        "Chapter not found",
      );
      const findBySlugMock = vi
        .fn()
        .mockReturnValue(err(notFoundError).toAsync());

      const workflow = createChapterFindBySlugWorkflow(validateSlug)(
        mockLogger,
      )(findBySlugMock)(createPassthroughFilter());

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(notFoundError);
    });

    it("ロガーがワークフロー開始時に呼ばれる", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(2);
      const findBySlugMock = vi.fn().mockReturnValue(ok(chapter).toAsync());
      const infoSpy = vi.spyOn(mockLogger, "info");

      const workflow = createChapterFindBySlugWorkflow(validateSlug)(
        mockLogger,
      )(findBySlugMock)(createPassthroughFilter());

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug: chapter.slug },
      };

      await workflow(command).unwrap();

      expect(infoSpy).toHaveBeenCalledWith(
        "ChapterFindBySlugWorkflow started",
        expect.objectContaining({ slug: chapter.slug }),
      );
    });

    it("ロガーがワークフロー完了時に呼ばれる", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(3);
      const findBySlugMock = vi.fn().mockReturnValue(ok(chapter).toAsync());
      const infoSpy = vi.spyOn(mockLogger, "info");

      const workflow = createChapterFindBySlugWorkflow(validateSlug)(
        mockLogger,
      )(findBySlugMock)(createPassthroughFilter());

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug: chapter.slug },
      };

      await workflow(command).unwrap();

      expect(infoSpy).toHaveBeenCalledWith(
        "ChapterFindBySlugWorkflow completed",
        expect.objectContaining({ identifier: chapter.identifier }),
      );
    });

    it("バリデーション失敗時にロガーの警告が呼ばれる", async () => {
      const findBySlugMock = vi.fn();
      const warnSpy = vi.spyOn(mockLogger, "warn");

      const workflow = createChapterFindBySlugWorkflow(validateSlug)(
        mockLogger,
      )(findBySlugMock)(createPassthroughFilter());

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug: "Invalid Slug With Spaces" },
      };

      await workflow(command).match({ ok: () => null, err: () => null });

      expect(warnSpy).toHaveBeenCalledWith(
        "Validation failed",
        expect.objectContaining({ error: expect.anything() }),
      );
    });

    it("createPublishedOnlyFilterでdraft状態のChapterはAggregateNotFoundErrorを返す", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(1, {
        status: PublishStatus.DRAFT,
      });
      const findBySlugMock = vi.fn().mockReturnValue(ok(chapter).toAsync());

      const workflow = createChapterFindBySlugWorkflow(validateSlug)(
        mockLogger,
      )(findBySlugMock)(createPublishedOnlyFilter("Chapter"));

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug: chapter.slug },
      };

      const error = await workflow(command).unwrapError();

      expect(isAggregateNotFoundError(error)).toBe(true);
    });

    it("createPublishedOnlyFilterでarchived状態のChapterはAggregateNotFoundErrorを返す", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(1, {
        status: PublishStatus.ARCHIVED,
      });
      const findBySlugMock = vi.fn().mockReturnValue(ok(chapter).toAsync());

      const workflow = createChapterFindBySlugWorkflow(validateSlug)(
        mockLogger,
      )(findBySlugMock)(createPublishedOnlyFilter("Chapter"));

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug: chapter.slug },
      };

      const error = await workflow(command).unwrapError();

      expect(isAggregateNotFoundError(error)).toBe(true);
    });

    it("createPublishedOnlyFilterでpublished状態のChapterは正常に返す", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(1, {
        status: PublishStatus.PUBLISHED,
      });
      const findBySlugMock = vi.fn().mockReturnValue(ok(chapter).toAsync());

      const workflow = createChapterFindBySlugWorkflow(validateSlug)(
        mockLogger,
      )(findBySlugMock)(createPublishedOnlyFilter("Chapter"));

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug: chapter.slug },
      };

      const result = await workflow(command).unwrap();

      expect(result).toEqual(chapter);
    });
  });

  describe("createChapterPersistWorkflow", () => {
    it("有効なチャプターでChapterPersistedEventを返す", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(1);
      const persistMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createChapterPersistWorkflow(validateChapter)(
        persistMock,
      )(mockLogger);

      const result = await workflow({
        identifier: chapter.identifier,
        title: chapter.title,
        slug: chapter.slug,
        content: chapter.content,
        images: chapter.images,
        status: chapter.status,
        publishedAt: chapter.publishedAt,
        timeline: chapter.timeline,
      }).unwrap();

      expect(result.type).toBe("chapter.persisted");
      expect(result.payload.chapter).toBe(chapter.identifier);
      expect(persistMock).toHaveBeenCalled();
    });

    it("永続化でエラーが発生した場合はエラーを返す", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(2);
      const error = aggregateNotFoundError("Chapter", "Chapter not found");
      const persistMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createChapterPersistWorkflow(validateChapter)(
        persistMock,
      )(mockLogger);

      const result = await workflow({
        identifier: chapter.identifier,
        title: chapter.title,
        slug: chapter.slug,
        content: chapter.content,
        images: chapter.images,
        status: chapter.status,
        publishedAt: chapter.publishedAt,
        timeline: chapter.timeline,
      }).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createChapterTerminateWorkflow", () => {
    it("有効なidentifierでChapterTerminatedEventを返す", async () => {
      const identifier = Forger(ChapterIdentifierMold).forgeWithSeed(1);
      const terminateMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createChapterTerminateWorkflow(validateChapterIdentifier)(
        terminateMock,
      )(mockLogger);

      const result = await workflow(identifier).unwrap();

      expect(result.type).toBe("chapter.terminated");
      expect(result.payload.chapter).toBe(identifier);
      expect(terminateMock).toHaveBeenCalledWith(identifier);
    });

    it("有効なidentifierでチャプターを削除できる", async () => {
      const identifier = Forger(ChapterIdentifierMold).forgeWithSeed(1);
      const terminateMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createChapterTerminateWorkflow(validateChapterIdentifier)(
        terminateMock,
      )(mockLogger);

      const result = await workflow(identifier);

      expect(await result.match({ ok: () => true, err: () => false })).toBe(true);
      expect(terminateMock).toHaveBeenCalledWith(identifier);
    });

    it("無効なidentifierでValidationErrorを返す", async () => {
      const terminateMock = vi.fn();

      const workflow = createChapterTerminateWorkflow(validateChapterIdentifier)(
        terminateMock,
      )(mockLogger);

      const result = await workflow("invalid-identifier").match({
        ok: () => false,
        err: () => true,
      });

      expect(result).toBe(true);
      expect(terminateMock).not.toHaveBeenCalled();
    });

    it("チャプターが見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const identifier = Forger(ChapterIdentifierMold).forgeWithSeed(2);
      const notFoundError = aggregateNotFoundError(
        "Chapter",
        "Chapter not found",
      );
      const terminateMock = vi
        .fn()
        .mockReturnValue(err(notFoundError).toAsync());

      const workflow = createChapterTerminateWorkflow(validateChapterIdentifier)(
        terminateMock,
      )(mockLogger);

      const result = await workflow(identifier).unwrapError();

      expect(result).toEqual(notFoundError);
    });

    it("ロガーがワークフロー開始時に呼ばれる", async () => {
      const identifier = Forger(ChapterIdentifierMold).forgeWithSeed(3);
      const terminateMock = vi.fn().mockReturnValue(ok(undefined).toAsync());
      const infoSpy = vi.spyOn(mockLogger, "info");

      const workflow = createChapterTerminateWorkflow(validateChapterIdentifier)(
        terminateMock,
      )(mockLogger);

      await workflow(identifier).match({ ok: () => null, err: () => null });

      expect(infoSpy).toHaveBeenCalledWith(
        "ChapterTerminateWorkflow started",
        expect.objectContaining({ identifier }),
      );
    });

    it("ロガーがワークフロー完了時に呼ばれる", async () => {
      const identifier = Forger(ChapterIdentifierMold).forgeWithSeed(4);
      const terminateMock = vi.fn().mockReturnValue(ok(undefined).toAsync());
      const infoSpy = vi.spyOn(mockLogger, "info");

      const workflow = createChapterTerminateWorkflow(validateChapterIdentifier)(
        terminateMock,
      )(mockLogger);

      await workflow(identifier).match({ ok: () => null, err: () => null });

      expect(infoSpy).toHaveBeenCalledWith(
        "ChapterTerminateWorkflow completed",
        expect.objectContaining({
          event: expect.objectContaining({
            type: "chapter.terminated",
            payload: expect.objectContaining({ chapter: identifier }),
          }),
        }),
      );
    });
  });

  describe("createChapterPersistWithSeriesWorkflow", () => {
    it("チャプター永続化とシリーズへの新規追加が正常に完了する", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(1);
      const series = Forger(SeriesMold).forgeWithSeed(1, { chapters: [] });
      const slug = series.slug;

      const persistChapterMock: ChapterPersistWorkflow = vi.fn().mockReturnValue(
        ok({ type: "chapter.persisted", payload: { chapter: chapter.identifier } }).toAsync(),
      );
      const findSeriesBySlugMock = vi.fn().mockReturnValue(ok(series).toAsync());
      const persistSeriesMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createChapterPersistWithSeriesWorkflow(persistChapterMock)(validateSlug)(
        findSeriesBySlugMock,
      )(persistSeriesMock)(mockLogger);

      const result = await workflow(
        {
          identifier: chapter.identifier,
          title: chapter.title,
          slug: chapter.slug,
          content: chapter.content,
          images: chapter.images,
          status: chapter.status,
          publishedAt: chapter.publishedAt,
          timeline: chapter.timeline,
        },
        slug,
      );

      expect(await result.match({ ok: () => true, err: () => false })).toBe(true);
      expect(persistChapterMock).toHaveBeenCalled();
      expect(findSeriesBySlugMock).toHaveBeenCalledWith(slug);
      expect(persistSeriesMock).toHaveBeenCalled();
    });

    it("チャプターが既にシリーズに含まれている場合はシリーズ更新をスキップする", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(2);
      const series = Forger(SeriesMold).forgeWithSeed(2, {
        chapters: [chapter.identifier],
      });
      const slug = series.slug;

      const persistChapterMock: ChapterPersistWorkflow = vi.fn().mockReturnValue(
        ok({ type: "chapter.persisted", payload: { chapter: chapter.identifier } }).toAsync(),
      );
      const findSeriesBySlugMock = vi.fn().mockReturnValue(ok(series).toAsync());
      const persistSeriesMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createChapterPersistWithSeriesWorkflow(persistChapterMock)(validateSlug)(
        findSeriesBySlugMock,
      )(persistSeriesMock)(mockLogger);

      const result = await workflow(
        {
          identifier: chapter.identifier,
          title: chapter.title,
          slug: chapter.slug,
          content: chapter.content,
          images: chapter.images,
          status: chapter.status,
          publishedAt: chapter.publishedAt,
          timeline: chapter.timeline,
        },
        slug,
      );

      expect(await result.match({ ok: () => true, err: () => false })).toBe(true);
      expect(persistSeriesMock).not.toHaveBeenCalled();
    });

    it("persistChapterが失敗した場合はエラーを返す", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(3);
      const error = aggregateNotFoundError("Chapter", "Chapter not found");
      const slug = Forger(SlugMold).forgeWithSeed(3);

      const persistChapterMock: ChapterPersistWorkflow = vi.fn().mockReturnValue(
        err(error).toAsync(),
      );
      const findSeriesBySlugMock = vi.fn();
      const persistSeriesMock = vi.fn();

      const workflow = createChapterPersistWithSeriesWorkflow(persistChapterMock)(validateSlug)(
        findSeriesBySlugMock,
      )(persistSeriesMock)(mockLogger);

      const result = await workflow(
        {
          identifier: chapter.identifier,
          title: chapter.title,
          slug: chapter.slug,
          content: chapter.content,
          images: chapter.images,
          status: chapter.status,
          publishedAt: chapter.publishedAt,
          timeline: chapter.timeline,
        },
        slug,
      ).unwrapError();

      expect(result).toEqual(error);
      expect(findSeriesBySlugMock).not.toHaveBeenCalled();
    });

    it("validateSlugが失敗した場合はエラーを返す", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(4);

      const persistChapterMock: ChapterPersistWorkflow = vi.fn().mockReturnValue(
        ok({ type: "chapter.persisted", payload: { chapter: chapter.identifier } }).toAsync(),
      );
      const findSeriesBySlugMock = vi.fn();
      const persistSeriesMock = vi.fn();

      const workflow = createChapterPersistWithSeriesWorkflow(persistChapterMock)(validateSlug)(
        findSeriesBySlugMock,
      )(persistSeriesMock)(mockLogger);

      const result = await workflow(
        {
          identifier: chapter.identifier,
          title: chapter.title,
          slug: chapter.slug,
          content: chapter.content,
          images: chapter.images,
          status: chapter.status,
          publishedAt: chapter.publishedAt,
          timeline: chapter.timeline,
        },
        "Invalid Slug With Spaces",
      );

      expect(await result.match({ ok: () => false, err: () => true })).toBe(true);
      expect(findSeriesBySlugMock).not.toHaveBeenCalled();
    });

    it("findSeriesBySlugが失敗した場合はエラーを返す", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(5);
      const error = aggregateNotFoundError("Series", "Series not found");
      const slug = Forger(SlugMold).forgeWithSeed(5);

      const persistChapterMock: ChapterPersistWorkflow = vi.fn().mockReturnValue(
        ok({ type: "chapter.persisted", payload: { chapter: chapter.identifier } }).toAsync(),
      );
      const findSeriesBySlugMock = vi.fn().mockReturnValue(err(error).toAsync());
      const persistSeriesMock = vi.fn();

      const workflow = createChapterPersistWithSeriesWorkflow(persistChapterMock)(validateSlug)(
        findSeriesBySlugMock,
      )(persistSeriesMock)(mockLogger);

      const result = await workflow(
        {
          identifier: chapter.identifier,
          title: chapter.title,
          slug: chapter.slug,
          content: chapter.content,
          images: chapter.images,
          status: chapter.status,
          publishedAt: chapter.publishedAt,
          timeline: chapter.timeline,
        },
        slug,
      ).unwrapError();

      expect(result).toEqual(error);
      expect(persistSeriesMock).not.toHaveBeenCalled();
    });

    it("subTitleとcoverがnullのシリーズでも正常に完了する", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(7);
      const baseSeries = Forger(SeriesMold).forgeWithSeed(7, { chapters: [] });
      const series = { ...baseSeries, subTitle: null, cover: null };
      const slug = series.slug;

      const persistChapterMock: ChapterPersistWorkflow = vi.fn().mockReturnValue(
        ok({ type: "chapter.persisted", payload: { chapter: chapter.identifier } }).toAsync(),
      );
      const findSeriesBySlugMock = vi.fn().mockReturnValue(ok(series).toAsync());
      const persistSeriesMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createChapterPersistWithSeriesWorkflow(persistChapterMock)(validateSlug)(
        findSeriesBySlugMock,
      )(persistSeriesMock)(mockLogger);

      const result = await workflow(
        {
          identifier: chapter.identifier,
          title: chapter.title,
          slug: chapter.slug,
          content: chapter.content,
          images: chapter.images,
          status: chapter.status,
          publishedAt: chapter.publishedAt,
          timeline: chapter.timeline,
        },
        slug,
      );

      expect(await result.match({ ok: () => true, err: () => false })).toBe(true);
      expect(persistSeriesMock).toHaveBeenCalled();
    });

    it("persistSeriesが失敗した場合はエラーを返す", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(6);
      const series = Forger(SeriesMold).forgeWithSeed(6, { chapters: [] });
      const error = aggregateNotFoundError("Series", "Series not found");
      const slug = series.slug;

      const persistChapterMock: ChapterPersistWorkflow = vi.fn().mockReturnValue(
        ok({ type: "chapter.persisted", payload: { chapter: chapter.identifier } }).toAsync(),
      );
      const findSeriesBySlugMock = vi.fn().mockReturnValue(ok(series).toAsync());
      const persistSeriesMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createChapterPersistWithSeriesWorkflow(persistChapterMock)(validateSlug)(
        findSeriesBySlugMock,
      )(persistSeriesMock)(mockLogger);

      const result = await workflow(
        {
          identifier: chapter.identifier,
          title: chapter.title,
          slug: chapter.slug,
          content: chapter.content,
          images: chapter.images,
          status: chapter.status,
          publishedAt: chapter.publishedAt,
          timeline: chapter.timeline,
        },
        slug,
      ).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createChapterTerminateWithSeriesWorkflow", () => {
    it("チャプター削除とシリーズからのチャプター除去が正常に完了する", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(1);
      const series = Forger(SeriesMold).forgeWithSeed(1, {
        chapters: [chapter.identifier],
      });
      const slug = series.slug;

      const terminateChapterMock: ChapterTerminateWorkflow = vi.fn().mockReturnValue(
        ok({ type: "chapter.terminated", payload: { chapter: chapter.identifier } }).toAsync(),
      );
      const findSeriesBySlugMock = vi.fn().mockReturnValue(ok(series).toAsync());
      const persistSeriesMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createChapterTerminateWithSeriesWorkflow(terminateChapterMock)(validateSlug)(
        findSeriesBySlugMock,
      )(persistSeriesMock)(mockLogger);

      const result = await workflow(chapter.identifier, slug);

      expect(await result.match({ ok: () => true, err: () => false })).toBe(true);
      expect(terminateChapterMock).toHaveBeenCalledWith(chapter.identifier);
      expect(findSeriesBySlugMock).toHaveBeenCalledWith(slug);
      expect(persistSeriesMock).toHaveBeenCalled();
    });

    it("terminateChapterが失敗した場合はエラーを返す", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(2);
      const error = aggregateNotFoundError("Chapter", "Chapter not found");
      const slug = Forger(SlugMold).forgeWithSeed(2);

      const terminateChapterMock: ChapterTerminateWorkflow = vi.fn().mockReturnValue(
        err(error).toAsync(),
      );
      const findSeriesBySlugMock = vi.fn();
      const persistSeriesMock = vi.fn();

      const workflow = createChapterTerminateWithSeriesWorkflow(terminateChapterMock)(validateSlug)(
        findSeriesBySlugMock,
      )(persistSeriesMock)(mockLogger);

      const result = await workflow(chapter.identifier, slug).unwrapError();

      expect(result).toEqual(error);
      expect(findSeriesBySlugMock).not.toHaveBeenCalled();
    });

    it("validateSlugが失敗した場合はエラーを返す", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(3);

      const terminateChapterMock: ChapterTerminateWorkflow = vi.fn().mockReturnValue(
        ok({ type: "chapter.terminated", payload: { chapter: chapter.identifier } }).toAsync(),
      );
      const findSeriesBySlugMock = vi.fn();
      const persistSeriesMock = vi.fn();

      const workflow = createChapterTerminateWithSeriesWorkflow(terminateChapterMock)(validateSlug)(
        findSeriesBySlugMock,
      )(persistSeriesMock)(mockLogger);

      const result = await workflow(chapter.identifier, "Invalid Slug With Spaces");

      expect(await result.match({ ok: () => false, err: () => true })).toBe(true);
      expect(findSeriesBySlugMock).not.toHaveBeenCalled();
    });

    it("findSeriesBySlugが失敗した場合はエラーを返す", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(4);
      const error = aggregateNotFoundError("Series", "Series not found");
      const slug = Forger(SlugMold).forgeWithSeed(4);

      const terminateChapterMock: ChapterTerminateWorkflow = vi.fn().mockReturnValue(
        ok({ type: "chapter.terminated", payload: { chapter: chapter.identifier } }).toAsync(),
      );
      const findSeriesBySlugMock = vi.fn().mockReturnValue(err(error).toAsync());
      const persistSeriesMock = vi.fn();

      const workflow = createChapterTerminateWithSeriesWorkflow(terminateChapterMock)(validateSlug)(
        findSeriesBySlugMock,
      )(persistSeriesMock)(mockLogger);

      const result = await workflow(chapter.identifier, slug).unwrapError();

      expect(result).toEqual(error);
      expect(persistSeriesMock).not.toHaveBeenCalled();
    });

    it("persistSeriesが失敗した場合はエラーを返す", async () => {
      const chapter = Forger(ChapterMold).forgeWithSeed(5);
      const series = Forger(SeriesMold).forgeWithSeed(5, {
        chapters: [chapter.identifier],
      });
      const error = aggregateNotFoundError("Series", "Series not found");
      const slug = series.slug;

      const terminateChapterMock: ChapterTerminateWorkflow = vi.fn().mockReturnValue(
        ok({ type: "chapter.terminated", payload: { chapter: chapter.identifier } }).toAsync(),
      );
      const findSeriesBySlugMock = vi.fn().mockReturnValue(ok(series).toAsync());
      const persistSeriesMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createChapterTerminateWithSeriesWorkflow(terminateChapterMock)(validateSlug)(
        findSeriesBySlugMock,
      )(persistSeriesMock)(mockLogger);

      const result = await workflow(chapter.identifier, slug).unwrapError();

      expect(result).toEqual(error);
    });
  });
});
