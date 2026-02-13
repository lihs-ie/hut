/**
 * @vitest-environment jsdom
 */
import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import { SeriesList } from "@shared/components/organisms/series/list";
import { Forger } from "@lihs-ie/forger-ts";
import { SeriesMold } from "../../../support/molds/domains/series";

const forgeSeriesListWithoutCover = (count: number, seed: number) =>
  Forger(SeriesMold).forgeMultiWithSeed(count, seed).map((series) => ({
    ...series,
    cover: null,
  }));

describe("components/organisms/series/SeriesList", () => {
  describe("表示", () => {
    it("シリーズ一覧が表示される", () => {
      const seriesList = forgeSeriesListWithoutCover(3, 1);

      render(<SeriesList seriesList={seriesList} />);

      for (const series of seriesList) {
        expect(screen.getByText(series.title)).toBeInTheDocument();
      }
    });

    it("空の場合に空状態メッセージが表示される", () => {
      render(<SeriesList seriesList={[]} />);

      expect(
        screen.getByText("シリーズがありません")
      ).toBeInTheDocument();
      expect(
        screen.getByText("シリーズが公開されるとここに表示されます")
      ).toBeInTheDocument();
    });
  });

  describe("構造", () => {
    it("ルート要素にcontainerクラスが適用される", () => {
      const seriesList = forgeSeriesListWithoutCover(2, 1);

      const { container } = render(
        <SeriesList seriesList={seriesList} />
      );

      const root = container.firstElementChild;
      expect(root?.className).toContain("container");
    });
  });
});
