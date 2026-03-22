import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  createChapterFindBySlugWorkflow,
  createChapterPersistWorkflow,
  createChapterTerminateWorkflow,
} from "@shared/workflows/chapter";
import { validateSlug } from "@shared/domains/common/slug";
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
      )(findBySlugMock);

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
      )(findBySlugMock);

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
      )(findBySlugMock);

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
      )(findBySlugMock);

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
      )(findBySlugMock);

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
      )(findBySlugMock);

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
});
