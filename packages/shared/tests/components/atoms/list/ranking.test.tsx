/**
 * @vitest-environment jsdom
 */
import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import { RankingRow } from "@shared/components/atoms/list/ranking";

describe("components/atoms/list/RankingRow", () => {
  const defaultProps = {
    rank: 1,
    label: "React入門ガイド",
    value: 320,
    maxValue: 320,
  };

  describe("表示", () => {
    it("ランクが表示される", () => {
      render(<RankingRow {...defaultProps} />);

      expect(screen.getByText("1")).toBeInTheDocument();
    });

    it("ラベルが表示される", () => {
      render(<RankingRow {...defaultProps} />);

      expect(screen.getByText("React入門ガイド")).toBeInTheDocument();
    });

    it("値が表示される", () => {
      render(<RankingRow {...defaultProps} />);

      expect(screen.getByText("320")).toBeInTheDocument();
    });

    it("文字列の値が表示される", () => {
      render(<RankingRow {...defaultProps} value="320 PV" />);

      expect(screen.getByText("320 PV")).toBeInTheDocument();
    });

    it("サブラベルが表示される", () => {
      render(<RankingRow {...defaultProps} subLabel="article" />);

      expect(screen.getByText("article")).toBeInTheDocument();
    });

    it("サブラベルが未指定の場合はサブラベル要素が表示されない", () => {
      const { container } = render(<RankingRow {...defaultProps} />);

      const subLabel = container.querySelector('[class*="sub"]');
      expect(subLabel).not.toBeInTheDocument();
    });
  });

  describe("プログレスバー", () => {
    it("maxValueに対する割合でバーの幅が設定される", () => {
      const { container } = render(
        <RankingRow {...defaultProps} value={160} maxValue={320} />
      );

      const fill = container.querySelector('[class*="fill"]');
      expect(fill).toBeInTheDocument();
      expect(fill).toHaveStyle({ width: "50%" });
    });

    it("maxValueが未指定の場合はバーの幅が0%になる", () => {
      const { container } = render(
        <RankingRow rank={1} label="テスト" value={100} />
      );

      const fill = container.querySelector('[class*="fill"]');
      expect(fill).toBeInTheDocument();
      expect(fill).toHaveStyle({ width: "0%" });
    });

    it("maxValueが0の場合はバーの幅が0%になる", () => {
      const { container } = render(
        <RankingRow {...defaultProps} value={100} maxValue={0} />
      );

      const fill = container.querySelector('[class*="fill"]');
      expect(fill).toHaveStyle({ width: "0%" });
    });
  });

  describe("構造", () => {
    it("ルート要素にcontainerクラスが適用される", () => {
      const { container } = render(<RankingRow {...defaultProps} />);

      const root = container.firstElementChild;
      expect(root?.className).toContain("container");
    });
  });
});
