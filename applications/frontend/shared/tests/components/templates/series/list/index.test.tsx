/**
 * @vitest-environment jsdom
 */
import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import { SeriesListIndex } from "@shared/components/templates/series/list/index";
import { Forger } from "@lihs-ie/forger-ts";
import { SeriesMold } from "../../../../support/molds/domains/series";

const forgeSeriesListWithoutCover = (count: number, seed: number) =>
  Forger(SeriesMold).forgeMultiWithSeed(count, seed).map((series) => ({
    ...series,
    cover: null,
  }));

describe("components/templates/series/list/SeriesListIndex", () => {
  describe("表示", () => {
    it("ページタイトルが表示される", () => {
      const seriesList = forgeSeriesListWithoutCover(3, 1);

      render(<SeriesListIndex seriesList={seriesList} />);

      expect(screen.getByText("シリーズ")).toBeInTheDocument();
    });

    it("説明文が表示される", () => {
      const seriesList = forgeSeriesListWithoutCover(3, 1);

      render(<SeriesListIndex seriesList={seriesList} />);

      expect(
        screen.getByText("技術書や体系的にまとめられたコンテンツ")
      ).toBeInTheDocument();
    });

    it("シリーズ一覧が表示される", () => {
      const seriesList = forgeSeriesListWithoutCover(3, 1);

      render(<SeriesListIndex seriesList={seriesList} />);

      for (const series of seriesList) {
        expect(screen.getByText(series.title)).toBeInTheDocument();
      }
    });
  });

  describe("構造", () => {
    it("ルート要素にcontainerクラスが適用される", () => {
      const seriesList = forgeSeriesListWithoutCover(2, 1);

      const { container } = render(
        <SeriesListIndex seriesList={seriesList} />
      );

      const root = container.firstElementChild;
      expect(root?.className).toContain("container");
    });
  });
});
