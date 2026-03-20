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

vi.mock("next/link", () => ({
  default: (linkProps: { href: string; children: React.ReactNode }) =>
    linkProps.children,
}));

vi.mock("@shared/components/atoms/icon/chevron-right", () => ({
  ChevronRightIcon: () => null,
}));

vi.mock("@shared/components/organisms/common/list/tag", () => ({
  TagBadgeList: () => null,
}));

describe("components/organisms/series/detail/SeriesDetail", () => {
  it("React 要素を返す", async () => {
    const series = Forger(SeriesMold).forge({
      chapters: [Forger(ChapterMold).forge()],
    });
    const slug = Forger(SeriesSlugMold).forge();

    const { SeriesDetail } = await import(
      "@shared/components/organisms/series/detail/index"
    );

    const result = SeriesDetail({
      series,
      slug,
      findAllTags: async () => [],
    });

    expect(isValidElement(result)).toBe(true);
  });

  it("複数の章があっても React 要素を返す", async () => {
    const chapters = [
      Forger(ChapterMold).forge({ title: "第1章" }),
      Forger(ChapterMold).forgeWithSeed(2, { title: "第2章" }),
    ];
    const series = Forger(SeriesMold).forge({ chapters });
    const slug = Forger(SeriesSlugMold).forge();

    const { SeriesDetail } = await import(
      "@shared/components/organisms/series/detail/index"
    );

    const result = SeriesDetail({
      series,
      slug,
      findAllTags: async () => [],
    });

    expect(isValidElement(result)).toBe(true);
  });
});
