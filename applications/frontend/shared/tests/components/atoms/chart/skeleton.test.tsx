/**
 * @vitest-environment jsdom
 */
import { describe, it, expect } from "vitest";
import { render } from "@testing-library/react";
import { ChartSkeleton } from "@shared/components/atoms/chart/skeleton";

describe("components/atoms/chart/ChartSkeleton", () => {
  describe("表示", () => {
    it("デフォルトの高さ(300px)でスケルトンが表示される", () => {
      const { container } = render(<ChartSkeleton />);

      const root = container.firstElementChild;
      expect(root).toHaveStyle({ height: "300px" });
    });

    it("指定した高さでスケルトンが表示される", () => {
      const { container } = render(<ChartSkeleton height={400} />);

      const root = container.firstElementChild;
      expect(root).toHaveStyle({ height: "400px" });
    });

    it("shimmerアニメーション要素が含まれる", () => {
      const { container } = render(<ChartSkeleton />);

      const shimmer = container.querySelector('[class*="shimmer"]');
      expect(shimmer).toBeInTheDocument();
    });
  });

  describe("構造", () => {
    it("ルート要素にcontainerクラスが適用される", () => {
      const { container } = render(<ChartSkeleton />);

      const root = container.firstElementChild;
      expect(root?.className).toContain("container");
    });
  });
});
