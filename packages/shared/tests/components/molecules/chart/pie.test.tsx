/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import React from "react";
import { PieChartPanel } from "@shared/components/molecules/chart/pie";

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
  const MockPieChart = (props: Record<string, unknown>) => (
    <div data-testid="recharts-pie-chart">
      {props.children as React.ReactNode}
    </div>
  );
  const MockPie = (props: Record<string, unknown>) => (
    <div data-testid="recharts-pie">
      {props.children as React.ReactNode}
    </div>
  );
  const MockCell = (props: Record<string, unknown>) => (
    <div data-testid="recharts-cell" data-fill={String(props.fill)} />
  );
  const MockTooltip = () => <div data-testid="recharts-tooltip" />;
  const MockLegend = () => <div data-testid="recharts-legend" />;

  return {
    ResponsiveContainer: MockResponsiveContainer,
    PieChart: MockPieChart,
    Pie: MockPie,
    Cell: MockCell,
    Tooltip: MockTooltip,
    Legend: MockLegend,
  };
});

describe("components/molecules/chart/PieChartPanel", () => {
  const sampleData = [
    { label: "Desktop", value: 65 },
    { label: "Mobile", value: 30 },
    { label: "Tablet", value: 5 },
  ];

  describe("表示", () => {
    it("タイトルが表示される", () => {
      render(<PieChartPanel title="デバイス比率" data={sampleData} />);

      expect(screen.getByText("デバイス比率")).toBeInTheDocument();
    });

    it("説明文が表示される", () => {
      render(
        <PieChartPanel
          title="デバイス比率"
          description="アクセス元デバイスの内訳"
          data={sampleData}
        />
      );

      expect(
        screen.getByText("アクセス元デバイスの内訳")
      ).toBeInTheDocument();
    });

    it("PieChartがレンダリングされる", async () => {
      render(<PieChartPanel title="デバイス比率" data={sampleData} />);

      expect(await screen.findByTestId("recharts-pie-chart")).toBeInTheDocument();
    });
  });

  describe("構造", () => {
    it("ルート要素にcontainerクラスが適用される", () => {
      const { container } = render(
        <PieChartPanel title="デバイス比率" data={sampleData} />
      );

      const root = container.firstElementChild;
      expect(root?.className).toContain("container");
    });
  });
});
