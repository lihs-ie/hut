/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import React from "react";
import { BarChartPanel } from "@shared/components/molecules/chart/bar";

vi.mock("next/dynamic", () => ({
  __esModule: true,
  default: (importFn: () => Promise<{ default: React.ComponentType }>) => {
    const LazyComponent = React.lazy(importFn);
    return function MockDynamic(props: Record<string, unknown>) {
      return (
        <React.Suspense fallback={null}>
          <LazyComponent {...props} />
        </React.Suspense>
      );
    };
  },
}));

vi.mock("recharts", () => {
  const MockResponsiveContainer = (props: Record<string, unknown>) => (
    <div data-testid="responsive-container" style={{ width: String(props.width), height: String(props.height) }}>
      {props.children as React.ReactNode}
    </div>
  );
  const MockBarChart = (props: Record<string, unknown>) => (
    <div data-testid="recharts-bar-chart">
      {props.children as React.ReactNode}
    </div>
  );
  const MockBar = () => <div data-testid="recharts-bar" />;
  const MockXAxis = () => <div data-testid="recharts-xaxis" />;
  const MockYAxis = () => <div data-testid="recharts-yaxis" />;
  const MockCartesianGrid = () => <div data-testid="recharts-grid" />;
  const MockTooltip = () => <div data-testid="recharts-tooltip" />;

  return {
    ResponsiveContainer: MockResponsiveContainer,
    BarChart: MockBarChart,
    Bar: MockBar,
    XAxis: MockXAxis,
    YAxis: MockYAxis,
    CartesianGrid: MockCartesianGrid,
    Tooltip: MockTooltip,
  };
});

describe("components/molecules/chart/BarChartPanel", () => {
  const sampleData = [
    { label: "React", value: 320 },
    { label: "Next.js", value: 210 },
  ];

  describe("表示", () => {
    it("タイトルが表示される", () => {
      render(<BarChartPanel title="タグ別PV" data={sampleData} />);

      expect(screen.getByText("タグ別PV")).toBeInTheDocument();
    });

    it("説明文が表示される", () => {
      render(
        <BarChartPanel
          title="タグ別PV"
          description="タグごとのPV集計"
          data={sampleData}
        />
      );

      expect(screen.getByText("タグごとのPV集計")).toBeInTheDocument();
    });

    it("BarChartがレンダリングされる", async () => {
      render(<BarChartPanel title="タグ別PV" data={sampleData} />);

      expect(await screen.findByTestId("recharts-bar-chart")).toBeInTheDocument();
    });
  });

  describe("構造", () => {
    it("ルート要素にcontainerクラスが適用される", () => {
      const { container } = render(
        <BarChartPanel title="タグ別PV" data={sampleData} />
      );

      const root = container.firstElementChild;
      expect(root?.className).toContain("container");
    });
  });
});
