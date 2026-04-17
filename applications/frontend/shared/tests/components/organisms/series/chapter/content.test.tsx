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
      chapters: [identifier],
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
      chapters: [identifier],
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

    let rendererStarted = false;
    let findChaptersStarted = false;
    let resolveRenderer!: (value: null) => void;
    let resolveFindChapters!: (value: typeof currentChapter[]) => void;

    const rendererPromise = new Promise<null>((resolve) => {
      resolveRenderer = resolve;
    });
    const findChaptersPromise = new Promise<typeof currentChapter[]>(
      (resolve) => {
        resolveFindChapters = resolve;
      },
    );

    const pending = ChapterContent({
      slug,
      chapterSlug,
      chapters: [identifier],
      renderer: () => {
        rendererStarted = true;
        return rendererPromise;
      },
      findChapterBySlug: async () => currentChapter,
      findChaptersByIdentifiers: () => {
        findChaptersStarted = true;
        return findChaptersPromise;
      },
    });

    while (!rendererStarted || !findChaptersStarted) {
      await Promise.resolve();
    }

    expect(rendererStarted).toBe(true);
    expect(findChaptersStarted).toBe(true);

    resolveRenderer(null);
    resolveFindChapters([currentChapter]);
    await pending;
  });
});
