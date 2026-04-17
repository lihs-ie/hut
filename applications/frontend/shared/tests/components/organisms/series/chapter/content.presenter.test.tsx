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

vi.mock("@shared/components/molecules/link/navigable", () => ({
  NavigableLink: (linkProps: {
    href: string;
    children: React.ReactNode;
    className?: string;
  }) => (
    <a href={linkProps.href} className={linkProps.className}>
      {linkProps.children}
    </a>
  ),
}));

vi.mock("@shared/components/atoms/icon/chevron-left", () => ({
  ChevronLeftIcon: () => null,
}));

vi.mock("@shared/components/atoms/icon/chevron-right", () => ({
  ChevronRightIcon: () => null,
}));

vi.mock("@shared/components/atoms/icon/clock", () => ({
  ClockIcon: () => null,
}));

vi.mock("@shared/components/atoms/text/modest", () => ({
  ModestText: (textProps: { children: React.ReactNode }) => (
    <span>{textProps.children}</span>
  ),
}));

describe("components/organisms/series/chapter/ChapterContentPresenter", () => {
  it("チャプタータイトルが h1 として表示される", async () => {
    const chapterSlug = Forger(ChapterSlugMold).forge();
    const currentChapter = Forger(ChapterMold).forge({
      slug: chapterSlug,
      title: "はじめに",
    });
    const chapters = [currentChapter, Forger(ChapterMold).forgeWithSeed(2)];
    const slug = Forger(SeriesSlugMold).forge();

    const { ChapterContentPresenter } = await import(
      "@shared/components/organisms/series/chapter/content.presenter"
    );

    const { unmount } = render(
      ChapterContentPresenter({
        slug,
        chapterSlug,
        currentChapter,
        currentIndex: 0,
        prevChapter: null,
        nextChapter: chapters[1],
        renderedContent: null,
      })
    );

    expect(screen.getByRole("heading", { level: 1 })).toHaveTextContent(
      "はじめに"
    );

    unmount();
  });

  it("チャプターラベルが「Chapter 01」のように2桁番号で表示される", async () => {
    const chapterSlug = Forger(ChapterSlugMold).forge();
    const currentChapter = Forger(ChapterMold).forge({ slug: chapterSlug });
    const slug = Forger(SeriesSlugMold).forge();

    const { ChapterContentPresenter } = await import(
      "@shared/components/organisms/series/chapter/content.presenter"
    );

    const { unmount } = render(
      ChapterContentPresenter({
        slug,
        chapterSlug,
        currentChapter,
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
    const nextChapter = Forger(ChapterMold).forgeWithSeed(2);
    const slug = Forger(SeriesSlugMold).forge();

    const { ChapterContentPresenter } = await import(
      "@shared/components/organisms/series/chapter/content.presenter"
    );

    const { unmount } = render(
      ChapterContentPresenter({
        slug,
        chapterSlug,
        currentChapter,
        currentIndex: 0,
        prevChapter: null,
        nextChapter,
        renderedContent: null,
      })
    );

    expect(screen.queryByText("前の章")).not.toBeInTheDocument();

    unmount();
  });

  it("最後のチャプターでは次の章ボタンが表示されない", async () => {
    const chapterSlug = Forger(ChapterSlugMold).forgeWithSeed(2);
    const currentChapter = Forger(ChapterMold).forgeWithSeed(2, {
      slug: chapterSlug,
    });
    const prevChapter = Forger(ChapterMold).forge();
    const slug = Forger(SeriesSlugMold).forge();

    const { ChapterContentPresenter } = await import(
      "@shared/components/organisms/series/chapter/content.presenter"
    );

    const { unmount } = render(
      ChapterContentPresenter({
        slug,
        chapterSlug,
        currentChapter,
        currentIndex: 1,
        prevChapter,
        nextChapter: null,
        renderedContent: null,
      })
    );

    expect(screen.queryByText("次の章")).not.toBeInTheDocument();

    unmount();
  });

  it("中間チャプターでは前の章と次の章の両方のボタンが表示される", async () => {
    const chapterSlug = Forger(ChapterSlugMold).forgeWithSeed(2);
    const currentChapter = Forger(ChapterMold).forgeWithSeed(2, {
      slug: chapterSlug,
    });
    const prevChapter = Forger(ChapterMold).forge();
    const nextChapter = Forger(ChapterMold).forgeWithSeed(3);
    const slug = Forger(SeriesSlugMold).forge();

    const { ChapterContentPresenter } = await import(
      "@shared/components/organisms/series/chapter/content.presenter"
    );

    const { unmount } = render(
      ChapterContentPresenter({
        slug,
        chapterSlug,
        currentChapter,
        currentIndex: 1,
        prevChapter,
        nextChapter,
        renderedContent: null,
      })
    );

    expect(screen.getByText("前の章")).toBeInTheDocument();
    expect(screen.getByText("次の章")).toBeInTheDocument();

    unmount();
  });

  it("前後のチャプターナビゲーションに aria-label が付与される", async () => {
    const chapterSlug = Forger(ChapterSlugMold).forge();
    const currentChapter = Forger(ChapterMold).forge({ slug: chapterSlug });
    const slug = Forger(SeriesSlugMold).forge();

    const { ChapterContentPresenter } = await import(
      "@shared/components/organisms/series/chapter/content.presenter"
    );

    const { unmount } = render(
      ChapterContentPresenter({
        slug,
        chapterSlug,
        currentChapter,
        currentIndex: 0,
        prevChapter: null,
        nextChapter: null,
        renderedContent: null,
      })
    );

    expect(
      screen.getByRole("navigation", { name: "前後のチャプター" })
    ).toBeInTheDocument();

    unmount();
  });
});
