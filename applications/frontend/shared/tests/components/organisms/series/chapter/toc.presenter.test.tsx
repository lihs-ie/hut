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

vi.mock("@shared/components/atoms/icon/facing-book", () => ({
  BookOpenIcon: () => null,
}));

vi.mock("@shared/components/molecules/link/chapter-toc-link", () => ({
  ChapterTocLink: (linkProps: {
    href: string;
    children: React.ReactNode;
    className?: string;
    activeClassName?: string;
  }) => <a href={linkProps.href}>{linkProps.children}</a>,
}));

describe("components/organisms/series/chapter/ChapterTocPresenter", () => {
  it("シリーズタイトルへのリンクが表示される", async () => {
    const slug = Forger(SeriesSlugMold).forge();
    const allChapters = [
      Forger(ChapterMold).forge({ title: "はじめに" }),
    ];

    const { ChapterTocPresenter } = await import(
      "@shared/components/organisms/series/chapter/toc.presenter"
    );

    const { unmount } = render(
      ChapterTocPresenter({
        slug,
        seriesTitle: "テストシリーズ",
        allChapters,
      })
    );

    expect(screen.getByText("テストシリーズ")).toBeInTheDocument();

    unmount();
  });

  it("チャプター一覧が表示される", async () => {
    const slug = Forger(SeriesSlugMold).forge();
    const chapterSlug = Forger(ChapterSlugMold).forge();
    const allChapters = [
      Forger(ChapterMold).forge({ slug: chapterSlug, title: "はじめに" }),
      Forger(ChapterMold).forgeWithSeed(2, { title: "基礎" }),
    ];

    const { ChapterTocPresenter } = await import(
      "@shared/components/organisms/series/chapter/toc.presenter"
    );

    const { unmount } = render(
      ChapterTocPresenter({
        slug,
        seriesTitle: "テストシリーズ",
        allChapters,
      })
    );

    expect(screen.getByText("はじめに")).toBeInTheDocument();
    expect(screen.getByText("基礎")).toBeInTheDocument();

    unmount();
  });

  it("チャプター番号が2桁ゼロ埋めで表示される", async () => {
    const slug = Forger(SeriesSlugMold).forge();
    const allChapters = [
      Forger(ChapterMold).forge({ title: "はじめに" }),
      Forger(ChapterMold).forgeWithSeed(2, { title: "基礎" }),
    ];

    const { ChapterTocPresenter } = await import(
      "@shared/components/organisms/series/chapter/toc.presenter"
    );

    const { unmount } = render(
      ChapterTocPresenter({
        slug,
        seriesTitle: "テストシリーズ",
        allChapters,
      })
    );

    expect(screen.getByText("01")).toBeInTheDocument();
    expect(screen.getByText("02")).toBeInTheDocument();

    unmount();
  });

  it("チャプター一覧のナビゲーションに aria-label が付与される", async () => {
    const slug = Forger(SeriesSlugMold).forge();
    const allChapters = [
      Forger(ChapterMold).forge({ title: "はじめに" }),
    ];

    const { ChapterTocPresenter } = await import(
      "@shared/components/organisms/series/chapter/toc.presenter"
    );

    const { unmount } = render(
      ChapterTocPresenter({
        slug,
        seriesTitle: "テストシリーズ",
        allChapters,
      })
    );

    expect(
      screen.getByRole("navigation", { name: "チャプター一覧" })
    ).toBeInTheDocument();

    unmount();
  });
});
