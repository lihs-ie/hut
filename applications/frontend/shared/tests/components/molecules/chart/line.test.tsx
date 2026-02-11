/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import React from "react";
import { LineChartPanel } from "@shared/components/molecules/chart/line";

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
  const MockLineChart = (props: Record<string, unknown>) => (
    <div data-testid="recharts-line-chart">
      {props.children as React.ReactNode}
    </div>
  );
  const MockLine = () => <div data-testid="recharts-line" />;
  const MockXAxis = () => <div data-testid="recharts-xaxis" />;
  const MockYAxis = () => <div data-testid="recharts-yaxis" />;
  const MockCartesianGrid = () => <div data-testid="recharts-grid" />;
  const MockTooltip = () => <div data-testid="recharts-tooltip" />;

  return {
    ResponsiveContainer: MockResponsiveContainer,
    LineChart: MockLineChart,
    Line: MockLine,
    XAxis: MockXAxis,
    YAxis: MockYAxis,
    CartesianGrid: MockCartesianGrid,
    Tooltip: MockTooltip,
  };
});

describe("components/molecules/chart/LineChartPanel", () => {
  const sampleData = [
    { label: "1/1", value: 100 },
    { label: "1/2", value: 200 },
  ];

  describe("表示", () => {
    it("タイトルが表示される", () => {
      render(<LineChartPanel title="PV推移" data={sampleData} />);

      expect(screen.getByText("PV推移")).toBeInTheDocument();
    });

    it("説明文が表示される", () => {
      render(
        <LineChartPanel
          title="PV推移"
          description="日別のページビュー数"
          data={sampleData}
        />
      );

      expect(screen.getByText("日別のページビュー数")).toBeInTheDocument();
    });

    it("LineChartがレンダリングされる", async () => {
      render(<LineChartPanel title="PV推移" data={sampleData} />);

      expect(await screen.findByTestId("recharts-line-chart")).toBeInTheDocument();
    });
  });

  describe("構造", () => {
    it("ルート要素にcontainerクラスが適用される", () => {
      const { container } = render(
        <LineChartPanel title="PV推移" data={sampleData} />
      );

      const root = container.firstElementChild;
      expect(root?.className).toContain("container");
    });

    it("SectionHeaderとLineChartの両方が含まれる", async () => {
      render(<LineChartPanel title="PV推移" data={sampleData} />);

      expect(screen.getByText("PV推移")).toBeInTheDocument();
      expect(await screen.findByTestId("recharts-line-chart")).toBeInTheDocument();
    });
  });
});
