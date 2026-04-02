/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { isValidElement } from "react";
import { Forger } from "@lihs-ie/forger-ts";
import {
  SeriesMold,
  SeriesSlugMold,
  ChapterSlugMold,
  ChapterIdentifierMold,
} from "../../../support/molds/domains/series";

vi.mock("@shared/components/molecules/skeleton", () => ({
  ArticleContentSkeleton: () => null,
}));

vi.mock("@shared/components/organisms/series/chapter", () => ({
  ChapterContainer: () => null,
}));

describe("components/templates/series/chapter/ChapterIndex", () => {
  it("React 要素を返す", async () => {
    const chapterSlug = Forger(ChapterSlugMold).forge();
    const chapterIdentifiers = [Forger(ChapterIdentifierMold).forge()];
    const series = Forger(SeriesMold).forge({ chapters: chapterIdentifiers });
    const slug = Forger(SeriesSlugMold).forge();

    const { ChapterIndex } = await import(
      "@shared/components/templates/series/chapter/index"
    );

    const result = ChapterIndex({
      slug,
      chapterSlug,
      series,
      renderer: async () => null,
      findChapterBySlug: async () => {
        throw new Error("not used");
      },
      findChaptersByIdentifiers: async () => [],
    });

    expect(isValidElement(result)).toBe(true);
  });
});
