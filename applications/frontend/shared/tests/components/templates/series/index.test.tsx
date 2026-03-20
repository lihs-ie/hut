/**
 * @vitest-environment node
 */
import { describe, it, expect, vi } from "vitest";
import { isValidElement } from "react";
import { Forger } from "@lihs-ie/forger-ts";
import { SeriesMold } from "@tests/support/molds/domains/series";

vi.mock("next/image", () => ({
  default: () => null,
}));

vi.mock("next/link", () => ({
  default: (props: { href: string; children: React.ReactNode }) => props.children,
}));

vi.mock("@shared/components/atoms/badge/simple", () => ({
  SimpleBadge: () => null,
}));

vi.mock("@shared/components/atoms/icon/facing-book", () => ({
  BookOpenIcon: () => null,
}));

vi.mock("@shared/components/atoms/icon/chevron-right", () => ({
  ChevronRightIcon: () => null,
}));

vi.mock("@shared/components/molecules/skeleton", () => ({
  ArticleContentSkeleton: () => null,
}));

vi.mock("@shared/components/organisms/series/detail", () => ({
  SeriesDetail: () => null,
}));

describe("components/templates/series/SeriesIndex", () => {
  it("author プロパティを受け取り React 要素を返す", async () => {
    const series = Forger(SeriesMold).forge();
    const { SeriesIndex } = await import(
      "@shared/components/templates/series/index"
    );

    const result = await SeriesIndex({
      slug: series.slug,
      findBySlug: async () => series,
      author: { name: "テスト著者", avatar: undefined, bio: undefined },
    });

    expect(isValidElement(result)).toBe(true);
  });

  it("author なしでも React 要素を返す", async () => {
    const series = Forger(SeriesMold).forge();
    const { SeriesIndex } = await import(
      "@shared/components/templates/series/index"
    );

    const result = await SeriesIndex({
      slug: series.slug,
      findBySlug: async () => series,
    });

    expect(isValidElement(result)).toBe(true);
  });
});
