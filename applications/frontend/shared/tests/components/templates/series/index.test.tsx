/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { isValidElement } from "react";
import { Forger } from "@lihs-ie/forger-ts";
import { SeriesMold } from "@tests/support/molds/domains/series";

vi.mock("@shared/components/molecules/skeleton", () => ({
  ArticleContentSkeleton: () => null,
}));

vi.mock("@shared/components/organisms/series/detail", () => ({
  SeriesDetail: () => null,
}));

describe("components/templates/series/SeriesIndex", () => {
  it("React 要素を返す", async () => {
    const series = Forger(SeriesMold).forge();
    const { SeriesIndex } = await import(
      "@shared/components/templates/series/index"
    );

    const result = await SeriesIndex({
      slug: series.slug,
      findBySlug: async () => series,
      findAllTags: async () => [],
    });

    expect(isValidElement(result)).toBe(true);
  });
});
