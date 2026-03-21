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
} from "../../../../support/molds/domains/series";

vi.mock(
  "@shared/components/organisms/series/detail/index.presenter",
  () => ({
    SeriesDetailPresenter: () => null,
  })
);

describe("components/organisms/series/detail/SeriesDetail (container)", () => {
  it("findBySlug と findChaptersByIdentifiers と findAllTags を呼び出してJSXを返す", async () => {
    const series = Forger(SeriesMold).forge();
    const chapters = [Forger(ChapterMold).forge()];
    const slug = Forger(SeriesSlugMold).forge();

    const findBySlug = vi.fn().mockResolvedValue(series);
    const findChaptersByIdentifiers = vi.fn().mockResolvedValue(chapters);
    const findAllTags = vi.fn().mockResolvedValue([]);

    const { SeriesDetail } = await import(
      "@shared/components/organisms/series/detail/index"
    );

    const result = await SeriesDetail({
      slug,
      findBySlug,
      findChaptersByIdentifiers,
      findAllTags,
    });

    expect(isValidElement(result)).toBe(true);
    expect(findBySlug).toHaveBeenCalledWith(slug);
    expect(findChaptersByIdentifiers).toHaveBeenCalledWith(series.chapters);
    expect(findAllTags).toHaveBeenCalledWith(series.tags);
  });
});
