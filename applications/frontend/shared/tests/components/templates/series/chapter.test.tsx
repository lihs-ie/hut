/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import { Forger } from "@lihs-ie/forger-ts";
import {
  SeriesMold,
  SeriesSlugMold,
  ChapterMold,
  ChapterSlugMold,
} from "../../../support/molds/domains/series";

vi.mock("next/link", () => ({
  default: (linkProps: {
    href: string;
    children: React.ReactNode;
    className?: string;
  }) => <a href={linkProps.href} className={linkProps.className}>{linkProps.children}</a>,
}));

vi.mock("react", async (importOriginal) => {
  const actual = await importOriginal<typeof import("react")>();
  return {
    ...actual,
    Suspense: (suspenseProps: { children: React.ReactNode }) =>
      suspenseProps.children,
  };
});

vi.mock("@shared/components/atoms/icon/facing-book", () => ({
  BookOpenIcon: () => null,
}));

vi.mock("@shared/components/atoms/icon/chevron-left", () => ({
  ChevronLeftIcon: () => null,
}));

vi.mock("@shared/components/atoms/icon/chevron-right", () => ({
  ChevronRightIcon: () => null,
}));

vi.mock("@shared/components/molecules/skeleton", () => ({
  ArticleContentSkeleton: () => null,
}));

describe("components/templates/series/chapter/ChapterIndex", () => {
  describe("Chapter ラベル", () => {
    it("「Chapter 01」のように2桁番号付きラベルが表示される", async () => {
      const chapterSlug = Forger(ChapterSlugMold).forge();
      const chapters = [
        Forger(ChapterMold).forge({ slug: chapterSlug, title: "はじめに" }),
        Forger(ChapterMold).forgeWithSeed(2, { title: "基礎" }),
      ];
      const series = Forger(SeriesMold).forge({ chapters });
      const slug = Forger(SeriesSlugMold).forge();

      const { ChapterIndex } = await import(
        "@shared/components/templates/series/chapter/index"
      );

      const { unmount } = render(
        await ChapterIndex({
          slug,
          chapterSlug,
          series,
          renderer: async () => null,
        })
      );

      expect(screen.getByText("Chapter 01")).toBeInTheDocument();

      unmount();
    });

    it("3番目のチャプターでは「Chapter 03」と表示される", async () => {
      const thirdChapterSlug = Forger(ChapterSlugMold).forgeWithSeed(3);
      const chapters = [
        Forger(ChapterMold).forge({ title: "第1章" }),
        Forger(ChapterMold).forgeWithSeed(2, { title: "第2章" }),
        Forger(ChapterMold).forgeWithSeed(3, {
          slug: thirdChapterSlug,
          title: "第3章",
        }),
      ];
      const series = Forger(SeriesMold).forge({ chapters });
      const slug = Forger(SeriesSlugMold).forge();

      const { ChapterIndex } = await import(
        "@shared/components/templates/series/chapter/index"
      );

      const { unmount } = render(
        await ChapterIndex({
          slug,
          chapterSlug: thirdChapterSlug,
          series,
          renderer: async () => null,
        })
      );

      expect(screen.getByText("Chapter 03")).toBeInTheDocument();

      unmount();
    });
  });

  describe("サイドバー チャプターリスト", () => {
    it("チャプター番号が2桁でゼロ埋めされて表示される", async () => {
      const chapterSlug = Forger(ChapterSlugMold).forge();
      const chapters = [
        Forger(ChapterMold).forge({ slug: chapterSlug, title: "はじめに" }),
        Forger(ChapterMold).forgeWithSeed(2, { title: "基礎" }),
      ];
      const series = Forger(SeriesMold).forge({ chapters });
      const slug = Forger(SeriesSlugMold).forge();

      const { ChapterIndex } = await import(
        "@shared/components/templates/series/chapter/index"
      );

      const { unmount } = render(
        await ChapterIndex({
          slug,
          chapterSlug,
          series,
          renderer: async () => null,
        })
      );

      expect(screen.getByText("01")).toBeInTheDocument();
      expect(screen.getByText("02")).toBeInTheDocument();

      unmount();
    });
  });

  describe("ナビゲーション", () => {
    it("先頭チャプターでは PREV ボタンが表示されない", async () => {
      const chapterSlug = Forger(ChapterSlugMold).forge();
      const chapters = [
        Forger(ChapterMold).forge({ slug: chapterSlug, title: "はじめに" }),
        Forger(ChapterMold).forgeWithSeed(2, { title: "基礎" }),
      ];
      const series = Forger(SeriesMold).forge({ chapters });
      const slug = Forger(SeriesSlugMold).forge();

      const { ChapterIndex } = await import(
        "@shared/components/templates/series/chapter/index"
      );

      const { unmount } = render(
        await ChapterIndex({
          slug,
          chapterSlug,
          series,
          renderer: async () => null,
        })
      );

      expect(screen.queryByText("PREV")).not.toBeInTheDocument();

      unmount();
    });

    it("最後のチャプターでは NEXT ボタンが表示されない", async () => {
      const lastChapterSlug = Forger(ChapterSlugMold).forgeWithSeed(2);
      const chapters = [
        Forger(ChapterMold).forge({ title: "はじめに" }),
        Forger(ChapterMold).forgeWithSeed(2, {
          slug: lastChapterSlug,
          title: "おわりに",
        }),
      ];
      const series = Forger(SeriesMold).forge({ chapters });
      const slug = Forger(SeriesSlugMold).forge();

      const { ChapterIndex } = await import(
        "@shared/components/templates/series/chapter/index"
      );

      const { unmount } = render(
        await ChapterIndex({
          slug,
          chapterSlug: lastChapterSlug,
          series,
          renderer: async () => null,
        })
      );

      expect(screen.queryByText("NEXT")).not.toBeInTheDocument();

      unmount();
    });

    it("中間チャプターでは PREV と NEXT の両方が表示される", async () => {
      const middleChapterSlug = Forger(ChapterSlugMold).forgeWithSeed(2);
      const chapters = [
        Forger(ChapterMold).forge({ title: "はじめに" }),
        Forger(ChapterMold).forgeWithSeed(2, {
          slug: middleChapterSlug,
          title: "中間章",
        }),
        Forger(ChapterMold).forgeWithSeed(3, { title: "おわりに" }),
      ];
      const series = Forger(SeriesMold).forge({ chapters });
      const slug = Forger(SeriesSlugMold).forge();

      const { ChapterIndex } = await import(
        "@shared/components/templates/series/chapter/index"
      );

      const { unmount } = render(
        await ChapterIndex({
          slug,
          chapterSlug: middleChapterSlug,
          series,
          renderer: async () => null,
        })
      );

      expect(screen.getByText("PREV")).toBeInTheDocument();
      expect(screen.getByText("NEXT")).toBeInTheDocument();

      unmount();
    });
  });

  describe("チャプターが見つからない場合", () => {
    it("エラーメッセージが表示される", async () => {
      const series = Forger(SeriesMold).forge({ chapters: [] });
      const slug = Forger(SeriesSlugMold).forge();
      const chapterSlug = Forger(ChapterSlugMold).forge();

      const { ChapterIndex } = await import(
        "@shared/components/templates/series/chapter/index"
      );

      const { unmount } = render(
        await ChapterIndex({
          slug,
          chapterSlug,
          series,
          renderer: async () => null,
        })
      );

      expect(
        screen.getByText("チャプターが見つかりませんでした")
      ).toBeInTheDocument();

      unmount();
    });
  });
});
