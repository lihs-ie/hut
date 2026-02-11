import { describe, it, expect } from "vitest";
import { Forger, Mold, StringMold } from "@lihs-ie/forger-ts";
import {
  chapterIdentifierSchema,
  chapterTitleSchema,
  contentSchema,
  chapterSchema,
  validateChapter,
  type ChapterIdentifier,
  type Chapter,
} from "@shared/domains/series/chapter";
import { TimelineMold, DateMold } from "../../support/molds/domains/common/date";
import { SlugMold } from "../../support/molds/domains/common/slug";
import { ulid } from "ulid";
import type { Slug, Timeline } from "@shared/domains/common";
import {
  describeIdentifierSchema,
  describeStringLengthSchema,
} from "../../support/helpers";

type ChapterIdentifierProperties = {
  value: string;
};

const ChapterIdentifierMold = Mold<ChapterIdentifier, ChapterIdentifierProperties>({
  pour: (properties) => chapterIdentifierSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ?? ulid(Forger(DateMold).forgeWithSeed(seed).getTime()),
  }),
});

type ChapterWithIdentifierProperties = {
  identifier: ChapterIdentifier;
  title: string;
  slug: Slug;
  content: string;
  timeline: Timeline;
};

const ChapterWithIdentifierMold = Mold<Chapter, ChapterWithIdentifierProperties>({
  pour: (properties) =>
    chapterSchema.parse({
      identifier: properties.identifier,
      title: properties.title,
      slug: properties.slug,
      content: properties.content,
      timeline: properties.timeline,
    }),
  prepare: (overrides, seed) => ({
    identifier:
      overrides.identifier ?? Forger(ChapterIdentifierMold).forgeWithSeed(seed),
    title: overrides.title ?? Forger(StringMold(1, 100)).forgeWithSeed(seed),
    slug: overrides.slug ?? Forger(SlugMold).forgeWithSeed(seed),
    content: overrides.content ?? Forger(StringMold(1, 1000)).forgeWithSeed(seed),
    timeline: overrides.timeline ?? Forger(TimelineMold).forgeWithSeed(seed),
  }),
});

describe("domains/series/chapter", () => {
  describe("chapterIdentifierSchema", () => {
    describeIdentifierSchema(
      "ChapterIdentifier",
      chapterIdentifierSchema,
      () => Forger(ChapterIdentifierMold).forge(),
      (count) => Forger(ChapterIdentifierMold).forgeMulti(count),
    );
  });

  describe("chapterTitleSchema", () => {
    describeStringLengthSchema(
      "チャプタータイトル",
      chapterTitleSchema,
      1,
      100,
      {
        minLengthMessage: "Chapter title must be at least 1 character long",
        maxLengthMessage: "Chapter title must be at most 100 characters long",
      },
    );

    it("日本語を含むタイトルは有効", () => {
      const result = chapterTitleSchema.safeParse("第1章: はじめに");
      expect(result.success).toBe(true);
    });
  });

  describe("contentSchema", () => {
    describe("有効なコンテンツの検証", () => {
      it("1文字は有効", () => {
        const result = contentSchema.safeParse("a");
        expect(result.success).toBe(true);
      });

      it("長いコンテンツも有効", () => {
        const result = contentSchema.safeParse("a".repeat(10000));
        expect(result.success).toBe(true);
      });
    });

    describe("無効なコンテンツの検証", () => {
      it("空文字列は無効", () => {
        const result = contentSchema.safeParse("");
        expect(result.success).toBe(false);
        if (!result.success) {
          expect(result.error.issues[0].message).toBe(
            "Chapter content must be at least 1 character long",
          );
        }
      });
    });
  });

  describe("chapterSchema", () => {
    describe("有効なChapterの検証", () => {
      it("全てのフィールドが有効な場合は検証を通過する", () => {
        const result = chapterSchema.safeParse(
          Forger(ChapterWithIdentifierMold).forge(),
        );
        expect(result.success).toBe(true);
      });
    });

    describe("無効なChapterの検証", () => {
      const createChapterWithOverrides = (
        overrides: Record<string, unknown>,
      ) => ({
        identifier: Forger(ChapterIdentifierMold).forge(),
        title: "Chapter Title",
        slug: Forger(SlugMold).forge(),
        content: "Content",
        timeline: Forger(TimelineMold).forge(),
        ...overrides,
      });

      it("identifierが無効な場合は無効", () => {
        const result = chapterSchema.safeParse(
          createChapterWithOverrides({ identifier: "invalid" }),
        );
        expect(result.success).toBe(false);
      });

      it("titleが空の場合は無効", () => {
        const result = chapterSchema.safeParse(
          createChapterWithOverrides({ title: "" }),
        );
        expect(result.success).toBe(false);
      });

      it("contentが空の場合は無効", () => {
        const result = chapterSchema.safeParse(
          createChapterWithOverrides({ content: "" }),
        );
        expect(result.success).toBe(false);
      });

      it("slugが無効な場合は無効", () => {
        const result = chapterSchema.safeParse(
          createChapterWithOverrides({ slug: "Invalid Slug" }),
        );
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateChapter", () => {
    it("有効なChapterでokを返す", () => {
      const result = validateChapter({
        identifier: Forger(ChapterIdentifierMold).forge(),
        title: "第1章: はじめに",
        slug: "chapter-1",
        content: "これはチャプターの内容です。",
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なChapterでerrを返す", () => {
      const result = validateChapter({
        identifier: "invalid",
        title: "",
        slug: "",
        content: "",
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isErr).toBe(true);
    });

    it("identifierが無効な場合はerrを返す", () => {
      const result = validateChapter({
        identifier: "not-a-ulid",
        title: "Valid Title",
        slug: "valid-slug",
        content: "Valid content",
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isErr).toBe(true);
    });

    it("titleが長すぎる場合はerrを返す", () => {
      const result = validateChapter({
        identifier: Forger(ChapterIdentifierMold).forge(),
        title: "a".repeat(101),
        slug: "valid-slug",
        content: "Valid content",
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isErr).toBe(true);
    });
  });
});
