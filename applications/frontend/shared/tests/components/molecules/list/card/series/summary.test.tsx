/**
 * @vitest-environment jsdom
 */
import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import { SeriesSummaryCard } from "@shared/components/molecules/list/card/series/summary";
import { Forger } from "@lihs-ie/forger-ts";
import {
  SeriesMold,
  SeriesSlugMold,
} from "../../../../../support/molds/domains/series";

describe("components/molecules/list/card/series/SeriesSummaryCard", () => {
  const series = Forger(SeriesMold).forgeWithSeed(1);

  describe("表示", () => {
    it("タイトルが表示される", () => {
      render(
        <SeriesSummaryCard
          slug={series.slug}
          title={series.title}
          description={series.description}
          cover={null}
          tags={series.tags}
          chapterCount={series.chapters.length}
        />
      );

      expect(screen.getByText(series.title)).toBeInTheDocument();
    });

    it("説明が表示される", () => {
      render(
        <SeriesSummaryCard
          slug={series.slug}
          title={series.title}
          description="テスト説明文"
          cover={null}
          tags={series.tags}
          chapterCount={series.chapters.length}
        />
      );

      expect(screen.getByText("テスト説明文")).toBeInTheDocument();
    });

    it("チャプター数が表示される", () => {
      render(
        <SeriesSummaryCard
          slug={series.slug}
          title={series.title}
          description={series.description}
          cover={null}
          tags={series.tags}
          chapterCount={5}
        />
      );

      expect(screen.getByText("5")).toBeInTheDocument();
    });
  });

  describe("構造", () => {
    it("ルート要素にcontainerクラスが適用される", () => {
      const { container } = render(
        <SeriesSummaryCard
          slug={series.slug}
          title={series.title}
          description={series.description}
          cover={null}
          tags={series.tags}
          chapterCount={series.chapters.length}
        />
      );

      const root = container.firstElementChild;
      expect(root?.className).toContain("container");
    });

    it("シリーズ詳細ページへのリンクが設定される", () => {
      const slug = Forger(SeriesSlugMold).forgeWithSeed(1);

      render(
        <SeriesSummaryCard
          slug={slug}
          title={series.title}
          description={series.description}
          cover={null}
          tags={series.tags}
          chapterCount={series.chapters.length}
        />
      );

      const link = screen.getByRole("link");
      expect(link).toHaveAttribute("href", `/series/${slug}`);
    });
  });
});
