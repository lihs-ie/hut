/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import { Forger } from "@lihs-ie/forger-ts";
import {
  SeriesSlugMold,
  ChapterMold,
  ChapterSlugMold,
} from "../../../../support/molds/domains/series";

vi.mock("next/link", () => ({
  default: (linkProps: {
    href: string;
    children: React.ReactNode;
    className?: string;
  }) => <a href={linkProps.href} className={linkProps.className}>{linkProps.children}</a>,
}));

vi.mock("@shared/components/atoms/icon/facing-book", () => ({
  BookOpenIcon: () => null,
}));

vi.mock("@shared/components/atoms/icon/chevron-left", () => ({
  ChevronLeftIcon: () => null,
}));

vi.mock("@shared/components/atoms/icon/chevron-right", () => ({
  ChevronRightIcon: () => null,
}));

vi.mock("@shared/components/atoms/text/modest", () => ({
  ModestText: (textProps: { children: React.ReactNode }) => <span>{textProps.children}</span>,
}));

describe("components/organisms/series/chapter/ChapterPresenter", () => {
  it("チャプタータイトルが表示される", async () => {
    const chapterSlug = Forger(ChapterSlugMold).forge();
    const currentChapter = Forger(ChapterMold).forge({
      slug: chapterSlug,
      title: "はじめに",
    });
    const allChapters = [
      currentChapter,
      Forger(ChapterMold).forgeWithSeed(2, { title: "基礎" }),
    ];
    const slug = Forger(SeriesSlugMold).forge();

    const { ChapterPresenter } = await import(
      "@shared/components/organisms/series/chapter/index.presenter"
    );

    const { unmount } = render(
      ChapterPresenter({
        slug,
        chapterSlug,
        seriesTitle: "テストシリーズ",
        currentChapter,
        allChapters,
        currentIndex: 0,
        prevChapter: null,
        nextChapter: allChapters[1],
        renderedContent: null,
      })
    );

    expect(screen.getByRole("heading", { level: 1 })).toHaveTextContent("はじめに");

    unmount();
  });

  it("チャプターラベルが「Chapter 01」のように2桁番号で表示される", async () => {
    const chapterSlug = Forger(ChapterSlugMold).forge();
    const currentChapter = Forger(ChapterMold).forge({
      slug: chapterSlug,
      title: "はじめに",
    });
    const allChapters = [currentChapter];
    const slug = Forger(SeriesSlugMold).forge();

    const { ChapterPresenter } = await import(
      "@shared/components/organisms/series/chapter/index.presenter"
    );

    const { unmount } = render(
      ChapterPresenter({
        slug,
        chapterSlug,
        seriesTitle: "テストシリーズ",
        currentChapter,
        allChapters,
        currentIndex: 0,
        prevChapter: null,
        nextChapter: null,
        renderedContent: null,
      })
    );

    expect(screen.getByText("Chapter 01")).toBeInTheDocument();

    unmount();
  });

  it("先頭チャプターでは前の章ボタンが表示されない", async () => {
    const chapterSlug = Forger(ChapterSlugMold).forge();
    const currentChapter = Forger(ChapterMold).forge({ slug: chapterSlug });
    const allChapters = [
      currentChapter,
      Forger(ChapterMold).forgeWithSeed(2),
    ];
    const slug = Forger(SeriesSlugMold).forge();

    const { ChapterPresenter } = await import(
      "@shared/components/organisms/series/chapter/index.presenter"
    );

    const { unmount } = render(
      ChapterPresenter({
        slug,
        chapterSlug,
        seriesTitle: "テストシリーズ",
        currentChapter,
        allChapters,
        currentIndex: 0,
        prevChapter: null,
        nextChapter: allChapters[1],
        renderedContent: null,
      })
    );

    expect(screen.queryByText("前の章")).not.toBeInTheDocument();

    unmount();
  });

  it("最後のチャプターでは次の章ボタンが表示されない", async () => {
    const chapterSlug = Forger(ChapterSlugMold).forgeWithSeed(2);
    const currentChapter = Forger(ChapterMold).forgeWithSeed(2, { slug: chapterSlug });
    const allChapters = [
      Forger(ChapterMold).forge(),
      currentChapter,
    ];
    const slug = Forger(SeriesSlugMold).forge();

    const { ChapterPresenter } = await import(
      "@shared/components/organisms/series/chapter/index.presenter"
    );

    const { unmount } = render(
      ChapterPresenter({
        slug,
        chapterSlug,
        seriesTitle: "テストシリーズ",
        currentChapter,
        allChapters,
        currentIndex: 1,
        prevChapter: allChapters[0],
        nextChapter: null,
        renderedContent: null,
      })
    );

    expect(screen.queryByText("次の章")).not.toBeInTheDocument();

    unmount();
  });

  it("中間チャプターでは前の章と次の章の両方が表示される", async () => {
    const chapterSlug = Forger(ChapterSlugMold).forgeWithSeed(2);
    const currentChapter = Forger(ChapterMold).forgeWithSeed(2, { slug: chapterSlug });
    const allChapters = [
      Forger(ChapterMold).forge(),
      currentChapter,
      Forger(ChapterMold).forgeWithSeed(3),
    ];
    const slug = Forger(SeriesSlugMold).forge();

    const { ChapterPresenter } = await import(
      "@shared/components/organisms/series/chapter/index.presenter"
    );

    const { unmount } = render(
      ChapterPresenter({
        slug,
        chapterSlug,
        seriesTitle: "テストシリーズ",
        currentChapter,
        allChapters,
        currentIndex: 1,
        prevChapter: allChapters[0],
        nextChapter: allChapters[2],
        renderedContent: null,
      })
    );

    expect(screen.getByText("前の章")).toBeInTheDocument();
    expect(screen.getByText("次の章")).toBeInTheDocument();

    unmount();
  });

  it("サイドバーのチャプター番号が2桁でゼロ埋めされる", async () => {
    const chapterSlug = Forger(ChapterSlugMold).forge();
    const currentChapter = Forger(ChapterMold).forge({ slug: chapterSlug });
    const allChapters = [
      currentChapter,
      Forger(ChapterMold).forgeWithSeed(2),
    ];
    const slug = Forger(SeriesSlugMold).forge();

    const { ChapterPresenter } = await import(
      "@shared/components/organisms/series/chapter/index.presenter"
    );

    const { unmount } = render(
      ChapterPresenter({
        slug,
        chapterSlug,
        seriesTitle: "テストシリーズ",
        currentChapter,
        allChapters,
        currentIndex: 0,
        prevChapter: null,
        nextChapter: allChapters[1],
        renderedContent: null,
      })
    );

    expect(screen.getByText("01")).toBeInTheDocument();
    expect(screen.getByText("02")).toBeInTheDocument();

    unmount();
  });
});
