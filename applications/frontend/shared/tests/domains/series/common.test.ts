import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  seriesIdentifierSchema,
  validateSeriesIdentifier,
  titleSchema,
  subTitleSchema,
  descriptionSchema,
  cover,
  seriesSchema,
  validateSeries,
  addChapter,
  removeChapter,
  criteriaSchema,
  validateCriteria,
} from "@shared/domains/series";
import { SortByField, Order } from "@shared/domains/common/sort";
import { chapterIdentifierSchema } from "@shared/domains/series/chapter";
import { PublishStatus } from "@shared/domains/common";
import {
  SeriesMold,
  SeriesIdentifierMold,
} from "../../support/molds/domains/series";
import { TimelineMold } from "../../support/molds/domains/common/date";
import { TagIdentifierMold } from "../../support/molds/domains/attributes/tag";
import { SlugMold } from "../../support/molds/domains/common/slug";
import {
  describeIdentifierSchema,
  describeStringLengthSchema,
} from "../../support/helpers";
import { ulid } from "ulid";

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

      it("chaptersにChapterIdentifierの配列を含んでいても有効", () => {
        const chapterIdentifiers = [ulid(), ulid()].map((id) =>
          chapterIdentifierSchema.parse(id)
        );
        const result = seriesSchema.safeParse(
          Forger(SeriesMold).forge({ chapters: chapterIdentifiers })
        );
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
          status: PublishStatus.PUBLISHED,
          publishedAt: null,
          timeline: Forger(TimelineMold).forge(),
        });
        expect(result.success).toBe(true);
      });

      it("statusがdraftでも有効", () => {
        const result = seriesSchema.safeParse({
          identifier: Forger(SeriesIdentifierMold).forge(),
          title: "Series Title",
          slug: Forger(SlugMold).forge(),
          tags: [],
          subTitle: null,
          description: "Description",
          cover: null,
          chapters: [],
          status: PublishStatus.DRAFT,
          publishedAt: null,
          timeline: Forger(TimelineMold).forge(),
        });
        expect(result.success).toBe(true);
      });

      it("publishedAtがnullでも有効", () => {
        const result = seriesSchema.safeParse(
          Forger(SeriesMold).forge({ publishedAt: null }),
        );
        expect(result.success).toBe(true);
      });

      it("publishedAtがDateでも有効", () => {
        const publishedAt = new Date("2025-01-01T00:00:00Z");
        const result = seriesSchema.safeParse(
          Forger(SeriesMold).forge({ publishedAt }),
        );
        expect(result.success).toBe(true);
      });

      it("publishedAtを省略すると無効", () => {
        const result = seriesSchema.safeParse({
          identifier: Forger(SeriesIdentifierMold).forge(),
          title: "Series Title",
          slug: Forger(SlugMold).forge(),
          tags: [],
          subTitle: null,
          description: "Description",
          cover: null,
          chapters: [],
          status: PublishStatus.PUBLISHED,
          timeline: Forger(TimelineMold).forge(),
        });
        expect(result.success).toBe(false);
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
        status: PublishStatus.PUBLISHED,
        publishedAt: null,
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

      it("statusが無効な値の場合は無効", () => {
        const result = seriesSchema.safeParse(createSeriesWithOverrides({ status: "invalid-status" }));
        expect(result.success).toBe(false);
      });

      it("chaptersにULIDでない文字列が含まれる場合は無効", () => {
        const result = seriesSchema.safeParse(
          createSeriesWithOverrides({ chapters: ["not-a-ulid"] })
        );
        expect(result.success).toBe(false);
      });

      it("publishedAtが文字列の場合は無効", () => {
        const result = seriesSchema.safeParse(
          createSeriesWithOverrides({ publishedAt: "2025-01-01" }),
        );
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
        status: "published",
        publishedAt: null,
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isOk).toBe(true);
    });

    it("chaptersにULIDを含む場合もokを返す", () => {
      const result = validateSeries({
        identifier: Forger(SeriesIdentifierMold).forge(),
        title: "テストシリーズ",
        slug: "test-series",
        tags: [],
        subTitle: null,
        description: "説明",
        cover: "https://example.com/cover.png",
        chapters: [ulid(), ulid()],
        status: "published",
        publishedAt: null,
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
        status: "published",
        publishedAt: null,
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isErr).toBe(true);
    });

    it("publishedAt=null のUnvalidatedSeriesでokを返す", () => {
      const result = validateSeries({
        identifier: Forger(SeriesIdentifierMold).forge(),
        title: "下書きシリーズ",
        slug: "draft-series",
        tags: [],
        subTitle: null,
        description: "説明",
        cover: null,
        chapters: [],
        status: "draft",
        publishedAt: null,
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isOk).toBe(true);
    });

    it("publishedAt=null の下書きSeriesでokを返す", () => {
      const result = validateSeries({
        identifier: Forger(SeriesIdentifierMold).forge(),
        title: "下書きシリーズ",
        slug: "draft-series",
        tags: [],
        subTitle: null,
        description: "説明",
        cover: null,
        chapters: [],
        status: "draft",
        publishedAt: null,
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isOk).toBe(true);
    });
  });

  describe("addChapter", () => {
    it("シリーズに新しいChapterIdentifierを追加する", () => {
      const series = Forger(SeriesMold).forge({ chapters: [] });
      const newChapterIdentifier = chapterIdentifierSchema.parse(ulid());
      const updatedSeries = addChapter(series, newChapterIdentifier);

      expect(updatedSeries.chapters).toHaveLength(1);
      expect(updatedSeries.chapters[0]).toEqual(newChapterIdentifier);
    });

    it("既存のChapterIdentifierに追加される", () => {
      const existingIdentifiers = [ulid(), ulid()].map((id) =>
        chapterIdentifierSchema.parse(id)
      );
      const series = Forger(SeriesMold).forge({ chapters: existingIdentifiers });
      const newIdentifier = chapterIdentifierSchema.parse(ulid());
      const updatedSeries = addChapter(series, newIdentifier);

      expect(updatedSeries.chapters).toHaveLength(3);
      expect(updatedSeries.chapters[2]).toEqual(newIdentifier);
    });

    it("元のシリーズは変更されない（イミュータブル）", () => {
      const series = Forger(SeriesMold).forge({ chapters: [] });
      addChapter(series, chapterIdentifierSchema.parse(ulid()));
      expect(series.chapters).toHaveLength(0);
    });
  });

  describe("removeChapter", () => {
    it("identifierに一致するChapterIdentifierを削除する", () => {
      const identifier1 = chapterIdentifierSchema.parse(ulid());
      const identifier2 = chapterIdentifierSchema.parse(ulid());
      const series = Forger(SeriesMold).forge({ chapters: [identifier1, identifier2] });
      const updatedSeries = removeChapter(series, identifier1);

      expect(updatedSeries.chapters).toHaveLength(1);
      expect(updatedSeries.chapters[0]).toEqual(identifier2);
    });

    it("存在しないidentifierを指定した場合は変更なし", () => {
      const identifier1 = chapterIdentifierSchema.parse(ulid());
      const series = Forger(SeriesMold).forge({ chapters: [identifier1] });
      const nonExistentIdentifier = chapterIdentifierSchema.parse(ulid());
      const updatedSeries = removeChapter(series, nonExistentIdentifier);

      expect(updatedSeries.chapters).toHaveLength(1);
    });

    it("元のシリーズは変更されない（イミュータブル）", () => {
      const identifier1 = chapterIdentifierSchema.parse(ulid());
      const series = Forger(SeriesMold).forge({ chapters: [identifier1] });
      removeChapter(series, identifier1);
      expect(series.chapters).toHaveLength(1);
    });
  });

  describe("criteriaSchema", () => {
    describe("有効なCriteriaの検証", () => {
      it("全てnullで有効", () => {
        const result = criteriaSchema.safeParse({ slug: null, tags: null, status: null, freeWord: null });
        expect(result.success).toBe(true);
      });

      it("全てのフィールドを指定しても有効", () => {
        const result = criteriaSchema.safeParse({
          slug: Forger(SlugMold).forge(),
          tags: Forger(TagIdentifierMold).forgeMulti(2),
          status: "published",
          freeWord: "検索ワード",
        });
        expect(result.success).toBe(true);
      });

      it("statusのみ指定しても有効", () => {
        const result = criteriaSchema.safeParse({
          slug: null,
          tags: null,
          status: "draft",
          freeWord: null,
        });
        expect(result.success).toBe(true);
      });

      it("freeWordのみ指定しても有効", () => {
        const result = criteriaSchema.safeParse({
          slug: null,
          tags: null,
          status: null,
          freeWord: "キーワード",
        });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なCriteriaの検証", () => {
      it("無効なslugの場合は無効", () => {
        const result = criteriaSchema.safeParse({ slug: "Invalid Slug With Spaces", tags: null, status: null, freeWord: null });
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateCriteria", () => {
    it("有効なCriteriaでokを返す", () => {
      const result = validateCriteria({ slug: "test-slug", tags: null, status: null, freeWord: null });
      expect(result.isOk).toBe(true);
    });

    it("statusを指定した有効なCriteriaでokを返す", () => {
      const result = validateCriteria({ slug: null, tags: null, status: "published", freeWord: null });
      expect(result.isOk).toBe(true);
    });

    it("無効なCriteriaでerrを返す", () => {
      const result = validateCriteria({ slug: "Invalid Slug", tags: null, status: null, freeWord: null });
      expect(result.isErr).toBe(true);
    });
  });

  describe("criteriaSchema sortBy/order", () => {
    it("sortByにcreatedAtを指定できる", () => {
      const result = criteriaSchema.safeParse({
        slug: null,
        tags: null,
        status: null,
        freeWord: null,
        sortBy: SortByField.CREATED_AT,
        order: null,
      });
      expect(result.success).toBe(true);
    });

    it("sortByにupdatedAtを指定できる", () => {
      const result = criteriaSchema.safeParse({
        slug: null,
        tags: null,
        status: null,
        freeWord: null,
        sortBy: SortByField.UPDATED_AT,
        order: null,
      });
      expect(result.success).toBe(true);
    });

    it("orderにascを指定できる", () => {
      const result = criteriaSchema.safeParse({
        slug: null,
        tags: null,
        status: null,
        freeWord: null,
        sortBy: null,
        order: Order.ASC,
      });
      expect(result.success).toBe(true);
    });

    it("orderにdescを指定できる", () => {
      const result = criteriaSchema.safeParse({
        slug: null,
        tags: null,
        status: null,
        freeWord: null,
        sortBy: null,
        order: Order.DESC,
      });
      expect(result.success).toBe(true);
    });

    it("sortByとorderを省略しても有効", () => {
      const result = criteriaSchema.safeParse({ slug: null, tags: null, status: null, freeWord: null });
      expect(result.success).toBe(true);
    });

    it("不正なsortByは無効", () => {
      const result = criteriaSchema.safeParse({
        slug: null,
        tags: null,
        status: null,
        freeWord: null,
        sortBy: "invalid-field",
        order: null,
      });
      expect(result.success).toBe(false);
    });

    it("不正なorderは無効", () => {
      const result = criteriaSchema.safeParse({
        slug: null,
        tags: null,
        status: null,
        freeWord: null,
        sortBy: null,
        order: "invalid-order",
      });
      expect(result.success).toBe(false);
    });

    it("sortByとorderを指定して有効なCriteriaを生成できる", () => {
      const result = validateCriteria({
        slug: null,
        tags: null,
        status: null,
        freeWord: null,
        sortBy: "createdAt",
        order: "desc",
      });
      expect(result.isOk).toBe(true);
    });
  });
});
