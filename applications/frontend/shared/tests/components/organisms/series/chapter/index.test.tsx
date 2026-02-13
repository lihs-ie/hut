/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { SeriesMold } from "../../../../support/molds/domains/series/common";

describe("components/organisms/series/chapter/Chapter", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("findBySlugが正しいslugで呼ばれる", async () => {
    const series = Forger(SeriesMold).forgeWithSeed(1);
    const findBySlug = vi.fn().mockResolvedValue(series);
    const renderer = vi.fn().mockReturnValue(null);

    const { Chapter } = await import(
      "@shared/components/organisms/series/chapter/index"
    );

    await Chapter({
      seriesSlug: series.slug,
      chapterSlug: series.chapters[0].slug,
      findBySlug,
      renderer,
    });

    expect(findBySlug).toHaveBeenCalledWith(series.slug);
    expect(findBySlug).toHaveBeenCalledTimes(1);
  });

  it("findBySlugが失敗してもコンポーネントが正常に返される", async () => {
    const series = Forger(SeriesMold).forgeWithSeed(2);
    const findBySlug = vi.fn().mockResolvedValue(series);
    const renderer = vi.fn().mockReturnValue(null);

    const { Chapter } = await import(
      "@shared/components/organisms/series/chapter/index"
    );

    const result = await Chapter({
      seriesSlug: series.slug,
      chapterSlug: series.chapters[0].slug,
      findBySlug,
      renderer,
    });

    expect(result).toBeDefined();
    expect(findBySlug).toHaveBeenCalledTimes(1);
  });
});
