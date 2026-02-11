/**
 * @vitest-environment jsdom
 */
import { describe, it, expect } from "vitest";
import { render } from "@testing-library/react";
import { StatsCardSkeleton } from "@shared/components/atoms/card/stats.skeleton";

describe("components/atoms/card/StatsCardSkeleton", () => {
  describe("表示", () => {
    it("デフォルトで4つのスケルトンカードが表示される", () => {
      const { container } = render(<StatsCardSkeleton />);

      const cards = container.querySelectorAll('[class*="header"]');
      expect(cards).toHaveLength(4);
    });

    it("指定した数のスケルトンカードが表示される", () => {
      const { container } = render(<StatsCardSkeleton count={2} />);

      const cards = container.querySelectorAll('[class*="header"]');
      expect(cards).toHaveLength(2);
    });

    it("スケルトン要素が含まれる", () => {
      const { container } = render(<StatsCardSkeleton count={1} />);

      const skeletons = container.querySelectorAll('[class*="shimmer"]');
      expect(skeletons.length).toBeGreaterThan(0);
    });
  });
});
