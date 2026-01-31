import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  seriesIdentifierSchema,
  validateSeriesIdentifier,
  titleSchema,
  subTitleSchema,
  descriptionSchema,
  cover,
  chapterSchema,
  validateChapter,
  seriesSchema,
  validateSeries,
  addChapter,
  removeChapter,
  updateChapter,
  criteriaSchema,
  validateCriteria,
} from "@shared/domains/series";
import {
  SeriesMold,
  SeriesIdentifierMold,
  ChapterMold,
} from "../../support/molds/domains/series";
import { TimelineMold } from "../../support/molds/domains/common/date";
import { TagIdentifierMold } from "../../support/molds/domains/attributes/tag";
import { SlugMold } from "../../support/molds/domains/common/slug";
import {
  describeIdentifierSchema,
  describeStringLengthSchema,
} from "../../support/helpers";

describe("domains/series/common", () => {
  describe("seriesIdentifierSchema", () => {
    describeIdentifierSchema(
      "SeriesIdentifier",
      seriesIdentifierSchema,
      () => Forger(SeriesIdentifierMold).forge(),
      (count) => Forger(SeriesIdentifierMold).forgeMulti(count)
    );
  });

  describe("validateSeriesIdentifier", () => {
    it("有効なULIDでokを返す", () => {
      const result = validateSeriesIdentifier(Forger(SeriesIdentifierMold).forge());
      expect(result.isOk).toBe(true);
    });

    it("無効な文字列でerrを返す", () => {
      const result = validateSeriesIdentifier("invalid");
      expect(result.isErr).toBe(true);
      if (result.isErr) {
        expect(result.unwrapError().field).toBe("SeriesIdentifier");
      }
    });
  });

  describe("titleSchema", () => {
    describeStringLengthSchema("タイトル", titleSchema, 1, 100);

    it("日本語を含むタイトルは有効", () => {
      const result = titleSchema.safeParse("シリーズタイトル");
      expect(result.success).toBe(true);
    });
  });

  describe("subTitleSchema", () => {
    describe("有効なサブタイトルの検証", () => {
      it("undefinedは有効", () => {
        const result = subTitleSchema.safeParse(undefined);
        expect(result.success).toBe(true);
      });

      it("1文字のサブタイトルは有効", () => {
        const result = subTitleSchema.safeParse("A");
        expect(result.success).toBe(true);
      });

      it("200文字のサブタイトルは有効", () => {
        const result = subTitleSchema.safeParse("a".repeat(200));
        expect(result.success).toBe(true);
      });
    });

    describe("無効なサブタイトルの検証", () => {
      it("空文字列は無効", () => {
        const result = subTitleSchema.safeParse("");
        expect(result.success).toBe(false);
      });

      it("201文字以上は無効", () => {
        const result = subTitleSchema.safeParse("a".repeat(201));
        expect(result.success).toBe(false);
      });
    });
  });

  describe("descriptionSchema", () => {
    describe("有効な説明の検証", () => {
      it("undefinedは有効", () => {
        const result = descriptionSchema.safeParse(undefined);
        expect(result.success).toBe(true);
      });

      it("空文字列は有効", () => {
        const result = descriptionSchema.safeParse("");
        expect(result.success).toBe(true);
      });

      it("500文字の説明は有効", () => {
        const result = descriptionSchema.safeParse("a".repeat(500));
        expect(result.success).toBe(true);
      });
    });

    describe("無効な説明の検証", () => {
      it("501文字以上は無効", () => {
        const result = descriptionSchema.safeParse("a".repeat(501));
        expect(result.success).toBe(false);
      });
    });
  });

  describe("cover", () => {
    describe("有効なカバーURLの検証", () => {
      it("httpsのURLは有効", () => {
        const result = cover.safeParse("https://example.com/cover.png");
        expect(result.success).toBe(true);
      });

      it("httpのURLは有効", () => {
        const result = cover.safeParse("http://example.com/cover.png");
        expect(result.success).toBe(true);
      });
    });

    describe("無効なカバーURLの検証", () => {
      it("空文字列は無効", () => {
        const result = cover.safeParse("");
        expect(result.success).toBe(false);
      });

      it("URLでない文字列は無効", () => {
        const result = cover.safeParse("not-a-url");
        expect(result.success).toBe(false);
      });
    });
  });

  describe("chapterSchema", () => {
    describe("有効なChapterの検証", () => {
      it("全てのフィールドが有効な場合は検証を通過する", () => {
        const result = chapterSchema.safeParse(Forger(ChapterMold).forge());
        expect(result.success).toBe(true);
      });
    });

    describe("無効なChapterの検証", () => {
      const createChapterWithOverrides = (overrides: Record<string, unknown>) => ({
        title: "Chapter 1",
        slug: Forger(SlugMold).forge(),
        content: "Content",
        timeline: Forger(TimelineMold).forge(),
        ...overrides,
      });

      it("titleが空の場合は無効", () => {
        const result = chapterSchema.safeParse(createChapterWithOverrides({ title: "" }));
        expect(result.success).toBe(false);
      });

      it("contentが空の場合は無効", () => {
        const result = chapterSchema.safeParse(createChapterWithOverrides({ content: "" }));
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateChapter", () => {
    it("有効なChapterでokを返す", () => {
      const result = validateChapter({
        title: "Chapter 1",
        slug: "chapter-1",
        content: "This is chapter content",
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なChapterでerrを返す", () => {
      const result = validateChapter({
        title: "",
        slug: "",
        content: "",
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isErr).toBe(true);
    });
  });

  describe("seriesSchema", () => {
    describe("有効なSeriesの検証", () => {
      it("全てのフィールドが有効な場合は検証を通過する", () => {
        const result = seriesSchema.safeParse(Forger(SeriesMold).forge());
        expect(result.success).toBe(true);
      });

      it("chaptersが空配列でも有効", () => {
        const result = seriesSchema.safeParse(Forger(SeriesMold).forge({ chapters: [] }));
        expect(result.success).toBe(true);
      });

      it("coverがnullでも有効", () => {
        const result = seriesSchema.safeParse(Forger(SeriesMold).forge({ cover: null }));
        expect(result.success).toBe(true);
      });

      it("subTitleがnullでも有効", () => {
        const result = seriesSchema.safeParse({
          identifier: Forger(SeriesIdentifierMold).forge(),
          title: "Series Title",
          slug: Forger(SlugMold).forge(),
          tags: [],
          subTitle: null,
          description: "Description",
          cover: "https://example.com/cover.png",
          chapters: [],
          timeline: Forger(TimelineMold).forge(),
        });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なSeriesの検証", () => {
      const createSeriesWithOverrides = (overrides: Record<string, unknown>) => ({
        identifier: Forger(SeriesIdentifierMold).forge(),
        title: "Test",
        slug: Forger(SlugMold).forge(),
        tags: [],
        subTitle: null,
        description: "Description",
        cover: null,
        chapters: [],
        timeline: Forger(TimelineMold).forge(),
        ...overrides,
      });

      it("identifierが無効な場合は無効", () => {
        const result = seriesSchema.safeParse(createSeriesWithOverrides({ identifier: "invalid" }));
        expect(result.success).toBe(false);
      });

      it("titleが空の場合は無効", () => {
        const result = seriesSchema.safeParse(createSeriesWithOverrides({ title: "" }));
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateSeries", () => {
    it("有効なSeriesでokを返す", () => {
      const result = validateSeries({
        identifier: Forger(SeriesIdentifierMold).forge(),
        title: "テストシリーズ",
        slug: "test-series",
        tags: [],
        subTitle: null,
        description: "説明",
        cover: "https://example.com/cover.png",
        chapters: [],
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なSeriesでerrを返す", () => {
      const result = validateSeries({
        identifier: "invalid",
        title: "",
        slug: "",
        tags: [],
        subTitle: null,
        description: "a".repeat(501),
        chapters: [],
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isErr).toBe(true);
    });
  });

  describe("addChapter", () => {
    it("シリーズに新しいチャプターを追加する", () => {
      const series = Forger(SeriesMold).forge({ chapters: [] });
      const newChapter = Forger(ChapterMold).forge();
      const updatedSeries = addChapter(series, newChapter);

      expect(updatedSeries.chapters).toHaveLength(1);
      expect(updatedSeries.chapters[0]).toEqual(newChapter);
    });

    it("既存のチャプターに追加される", () => {
      const existingChapters = Forger(ChapterMold).forgeMulti(2);
      const series = Forger(SeriesMold).forge({ chapters: existingChapters });
      const newChapter = Forger(ChapterMold).forge();
      const updatedSeries = addChapter(series, newChapter);

      expect(updatedSeries.chapters).toHaveLength(3);
      expect(updatedSeries.chapters[2]).toEqual(newChapter);
    });

    it("timelineのupdatedAtがチャプターのtimeline.createdAtに更新される", () => {
      const series = Forger(SeriesMold).forge();
      const chapterDate = new Date("2025-12-31T00:00:00Z");
      const newChapter = Forger(ChapterMold).forge({
        timeline: { createdAt: chapterDate, updatedAt: chapterDate },
      });
      const updatedSeries = addChapter(series, newChapter);

      expect(updatedSeries.timeline.updatedAt).toEqual(chapterDate);
    });

    it("元のシリーズは変更されない（イミュータブル）", () => {
      const series = Forger(SeriesMold).forge({ chapters: [] });
      addChapter(series, Forger(ChapterMold).forge());
      expect(series.chapters).toHaveLength(0);
    });
  });

  describe("removeChapter", () => {
    it("タイトルに一致するチャプターを削除する", () => {
      const chapter1 = Forger(ChapterMold).forge({ title: "Chapter 1" });
      const chapter2 = Forger(ChapterMold).forge({ title: "Chapter 2" });
      const series = Forger(SeriesMold).forge({ chapters: [chapter1, chapter2] });
      const updatedSeries = removeChapter(series, "Chapter 1");

      expect(updatedSeries.chapters).toHaveLength(1);
      expect(updatedSeries.chapters[0].title).toBe("Chapter 2");
    });

    it("存在しないタイトルを指定した場合は変更なし", () => {
      const chapter1 = Forger(ChapterMold).forge({ title: "Chapter 1" });
      const series = Forger(SeriesMold).forge({ chapters: [chapter1] });
      const updatedSeries = removeChapter(series, "Non-existent");

      expect(updatedSeries.chapters).toHaveLength(1);
    });

    it("元のシリーズは変更されない（イミュータブル）", () => {
      const chapter1 = Forger(ChapterMold).forge({ title: "Chapter 1" });
      const series = Forger(SeriesMold).forge({ chapters: [chapter1] });
      removeChapter(series, "Chapter 1");
      expect(series.chapters).toHaveLength(1);
    });
  });

  describe("updateChapter", () => {
    it("タイトルに一致するチャプターを更新する", () => {
      const chapter1 = Forger(ChapterMold).forge({ title: "Chapter 1" });
      const chapter2 = Forger(ChapterMold).forge({ title: "Chapter 2" });
      const series = Forger(SeriesMold).forge({ chapters: [chapter1, chapter2] });
      const updatedChapter = { ...chapter1, content: "Updated content" };
      const updatedSeries = updateChapter(series, updatedChapter);

      expect(updatedSeries.chapters[0].content).toBe("Updated content");
      expect(updatedSeries.chapters[1]).toEqual(chapter2);
    });

    it("timelineのupdatedAtが更新される", () => {
      const chapter1 = Forger(ChapterMold).forge({ title: "Chapter 1" });
      const series = Forger(SeriesMold).forge({ chapters: [chapter1] });
      const newDate = new Date("2025-12-31T00:00:00Z");
      const updatedChapter = {
        ...chapter1,
        timeline: { ...chapter1.timeline, updatedAt: newDate },
      };
      const updatedSeries = updateChapter(series, updatedChapter);

      expect(updatedSeries.timeline.updatedAt).toEqual(newDate);
    });

    it("存在しないタイトルを指定した場合は変更なし", () => {
      const chapter1 = Forger(ChapterMold).forge({ title: "Chapter 1" });
      const series = Forger(SeriesMold).forge({ chapters: [chapter1] });
      const nonExistentChapter = Forger(ChapterMold).forge({ title: "Non-existent" });
      const updatedSeries = updateChapter(series, nonExistentChapter);

      expect(updatedSeries.chapters).toHaveLength(1);
      expect(updatedSeries.chapters[0]).toEqual(chapter1);
    });
  });

  describe("criteriaSchema", () => {
    describe("有効なCriteriaの検証", () => {
      it("全てnullで有効", () => {
        const result = criteriaSchema.safeParse({ slug: null, tags: null });
        expect(result.success).toBe(true);
      });

      it("全てのフィールドを指定しても有効", () => {
        const result = criteriaSchema.safeParse({
          slug: Forger(SlugMold).forge(),
          tags: Forger(TagIdentifierMold).forgeMulti(2),
        });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なCriteriaの検証", () => {
      it("無効なslugの場合は無効", () => {
        const result = criteriaSchema.safeParse({ slug: "Invalid Slug With Spaces", tags: null });
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateCriteria", () => {
    it("有効なCriteriaでokを返す", () => {
      const result = validateCriteria({ slug: "test-slug", tags: null });
      expect(result.isOk).toBe(true);
    });

    it("無効なCriteriaでerrを返す", () => {
      const result = validateCriteria({ slug: "Invalid Slug", tags: null });
      expect(result.isErr).toBe(true);
    });
  });
});
