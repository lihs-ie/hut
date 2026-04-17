/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { isValidElement } from "react";
import { Forger } from "@lihs-ie/forger-ts";
import {
  SeriesSlugMold,
  ChapterMold,
  ChapterSlugMold,
  ChapterIdentifierMold,
} from "../../../../support/molds/domains/series";

vi.mock("@shared/components/organisms/series/chapter/content.presenter", () => ({
  ChapterContentPresenter: () => null,
}));

describe("components/organisms/series/chapter/ChapterContent", () => {
  it("React 要素を返す", async () => {
    const slug = Forger(SeriesSlugMold).forge();
    const chapterSlug = Forger(ChapterSlugMold).forge();
    const identifier = Forger(ChapterIdentifierMold).forge();
    const currentChapter = Forger(ChapterMold).forge({ slug: chapterSlug });

    const { ChapterContent } = await import(
      "@shared/components/organisms/series/chapter/content"
    );

    const result = await ChapterContent({
      slug,
      chapterSlug,
      seriesChapterIdentifiers: [identifier],
      renderer: async () => null,
      findChapterBySlug: async () => currentChapter,
      findChaptersByIdentifiers: async () => [currentChapter],
    });

    expect(isValidElement(result)).toBe(true);
  });

  it("チャプターが見つからない場合はフォールバックを表示する", async () => {
    const slug = Forger(SeriesSlugMold).forge();
    const chapterSlug = Forger(ChapterSlugMold).forge();
    const identifier = Forger(ChapterIdentifierMold).forge();
    const currentChapter = Forger(ChapterMold).forge({ slug: chapterSlug });

    const { ChapterContent } = await import(
      "@shared/components/organisms/series/chapter/content"
    );

    const result = await ChapterContent({
      slug,
      chapterSlug,
      seriesChapterIdentifiers: [identifier],
      renderer: async () => null,
      findChapterBySlug: async () => currentChapter,
      findChaptersByIdentifiers: async () => [],
    });

    expect(isValidElement(result)).toBe(true);
  });

  it("currentChapter 取得後に findChaptersByIdentifiers と renderer を並列実行する", async () => {
    const slug = Forger(SeriesSlugMold).forge();
    const chapterSlug = Forger(ChapterSlugMold).forge();
    const identifier = Forger(ChapterIdentifierMold).forge();
    const currentChapter = Forger(ChapterMold).forge({ slug: chapterSlug });

    const { ChapterContent } = await import(
      "@shared/components/organisms/series/chapter/content"
    );

    const startTimes: Record<string, number> = {};
    const endTimes: Record<string, number> = {};

    const delay = (ms: number) =>
      new Promise((resolve) => setTimeout(resolve, ms));

    await ChapterContent({
      slug,
      chapterSlug,
      seriesChapterIdentifiers: [identifier],
      renderer: async () => {
        startTimes.renderer = Date.now();
        await delay(30);
        endTimes.renderer = Date.now();
        return null;
      },
      findChapterBySlug: async () => currentChapter,
      findChaptersByIdentifiers: async () => {
        startTimes.findChaptersByIdentifiers = Date.now();
        await delay(30);
        endTimes.findChaptersByIdentifiers = Date.now();
        return [currentChapter];
      },
    });

    const rendererStart = startTimes.renderer;
    const findChaptersStart = startTimes.findChaptersByIdentifiers;
    const rendererEnd = endTimes.renderer;
    const findChaptersEnd = endTimes.findChaptersByIdentifiers;

    expect(rendererStart).toBeLessThan(findChaptersEnd);
    expect(findChaptersStart).toBeLessThan(rendererEnd);
  });
});
