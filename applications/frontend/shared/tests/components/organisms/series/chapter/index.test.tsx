/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { isValidElement } from "react";
import { Forger } from "@lihs-ie/forger-ts";
import {
  SeriesMold,
  SeriesSlugMold,
  ChapterMold,
  ChapterSlugMold,
  ChapterIdentifierMold,
} from "../../../../support/molds/domains/series";

vi.mock(
  "@shared/components/organisms/series/chapter/index.presenter",
  () => ({
    ChapterPresenter: () => null,
  })
);

describe("components/organisms/series/chapter/ChapterContainer (container)", () => {
  it("findChapterBySlug と findChaptersByIdentifiers を呼び出してJSXを返す", async () => {
    const chapterSlug = Forger(ChapterSlugMold).forge();
    const chapters = [
      Forger(ChapterMold).forge({ slug: chapterSlug }),
      Forger(ChapterMold).forgeWithSeed(2),
    ];
    const chapterIdentifiers = chapters.map(() =>
      Forger(ChapterIdentifierMold).forge()
    );
    const series = Forger(SeriesMold).forge({ chapters: chapterIdentifiers });
    const slug = Forger(SeriesSlugMold).forge();

    const findChapterBySlug = vi.fn().mockResolvedValue(chapters[0]);
    const findChaptersByIdentifiers = vi.fn().mockResolvedValue(chapters);
    const renderer = vi.fn().mockResolvedValue(null);

    const { ChapterContainer } = await import(
      "@shared/components/organisms/series/chapter/index"
    );

    const result = await ChapterContainer({
      slug,
      chapterSlug,
      series,
      renderer,
      findChapterBySlug,
      findChaptersByIdentifiers,
    });

    expect(isValidElement(result)).toBe(true);
    expect(findChapterBySlug).toHaveBeenCalledWith(chapterSlug);
    expect(findChaptersByIdentifiers).toHaveBeenCalledWith(series.chapters);
  });

  it("チャプターが見つからない場合にエラーメッセージ要素を返す", async () => {
    const chapterSlug = Forger(ChapterSlugMold).forge();
    const series = Forger(SeriesMold).forge({ chapters: [] });
    const slug = Forger(SeriesSlugMold).forge();
    const notFoundChapter = Forger(ChapterMold).forge({
      slug: Forger(ChapterSlugMold).forge(),
    });

    const findChapterBySlug = vi.fn().mockResolvedValue(notFoundChapter);
    const findChaptersByIdentifiers = vi.fn().mockResolvedValue([]);
    const renderer = vi.fn().mockResolvedValue(null);

    const { ChapterContainer } = await import(
      "@shared/components/organisms/series/chapter/index"
    );

    const result = await ChapterContainer({
      slug,
      chapterSlug,
      series,
      renderer,
      findChapterBySlug,
      findChaptersByIdentifiers,
    });

    expect(isValidElement(result)).toBe(true);
  });
});
