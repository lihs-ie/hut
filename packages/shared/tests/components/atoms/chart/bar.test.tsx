/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render } from "@testing-library/react";
import { BarChart } from "@shared/components/atoms/chart/bar";

vi.mock("recharts", () => {
  const MockResponsiveContainer = (props: Record<string, unknown>) => (
    <div data-testid="responsive-container" data-height={String(props.height)}>
      {props.children as React.ReactNode}
    </div>
  );
  const MockBarChart = (props: Record<string, unknown>) => (
    <div data-testid="recharts-bar-chart" data-data={JSON.stringify(props.data)} data-layout={String(props.layout ?? "horizontal")}>
      {props.children as React.ReactNode}
    </div>
  );
  const MockBar = (props: Record<string, unknown>) => (
    <div data-testid="recharts-bar" data-fill={String(props.fill)} data-datakey={String(props.dataKey)} />
  );
  const MockXAxis = (props: Record<string, unknown>) => (
    <div data-testid="recharts-xaxis" data-datakey={String(props.dataKey)} data-type={String(props.type ?? "")} />
  );
  const MockYAxis = (props: Record<string, unknown>) => (
    <div data-testid="recharts-yaxis" data-datakey={String(props.dataKey ?? "")} data-type={String(props.type ?? "")} />
  );
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

describe("components/atoms/chart/BarChart", () => {
  const sampleData = [
    { label: "React", value: 320 },
    { label: "Next.js", value: 210 },
    { label: "TypeScript", value: 180 },
  ];

  describe("表示", () => {
    it("ResponsiveContainerがレンダリングされる", () => {
      const { getByTestId } = render(<BarChart data={sampleData} />);

      expect(getByTestId("responsive-container")).toBeInTheDocument();
    });

    it("RechartsのBarChartがレンダリングされる", () => {
      const { getByTestId } = render(<BarChart data={sampleData} />);

      expect(getByTestId("recharts-bar-chart")).toBeInTheDocument();
    });

    it("データがBarChartに渡される", () => {
      const { getByTestId } = render(<BarChart data={sampleData} />);

      const chart = getByTestId("recharts-bar-chart");
      expect(chart.dataset.data).toBe(JSON.stringify(sampleData));
    });
  });

  describe("プロパティ", () => {
    it("デフォルトの高さは300", () => {
      const { getByTestId } = render(<BarChart data={sampleData} />);

      const container = getByTestId("responsive-container");
      expect(container.dataset.height).toBe("300");
    });

    it("カスタムの高さが設定できる", () => {
      const { getByTestId } = render(
        <BarChart data={sampleData} height={400} />
      );

      const container = getByTestId("responsive-container");
      expect(container.dataset.height).toBe("400");
    });

    it("デフォルトのカラーはvar(--primary)", () => {
      const { getByTestId } = render(<BarChart data={sampleData} />);

      const bar = getByTestId("recharts-bar");
      expect(bar.dataset.fill).toBe("var(--primary)");
    });

    it("カスタムカラーが設定できる", () => {
      const { getByTestId } = render(
        <BarChart data={sampleData} color="var(--accent)" />
      );

      const bar = getByTestId("recharts-bar");
      expect(bar.dataset.fill).toBe("var(--accent)");
    });

    it("デフォルトのレイアウトはhorizontal", () => {
      const { getByTestId } = render(<BarChart data={sampleData} />);

      const chart = getByTestId("recharts-bar-chart");
      expect(chart.dataset.layout).toBe("horizontal");
    });

    it("verticalレイアウトが設定できる", () => {
      const { getByTestId } = render(
        <BarChart data={sampleData} layout="vertical" />
      );

      const chart = getByTestId("recharts-bar-chart");
      expect(chart.dataset.layout).toBe("vertical");
    });
  });

  describe("構造", () => {
    it("ルート要素にcontainerクラスが適用される", () => {
      const { container } = render(<BarChart data={sampleData} />);

      const root = container.firstElementChild;
      expect(root?.className).toContain("container");
    });

    it("CartesianGridが含まれる", () => {
      const { getByTestId } = render(<BarChart data={sampleData} />);

      expect(getByTestId("recharts-grid")).toBeInTheDocument();
    });

    it("Tooltipが含まれる", () => {
      const { getByTestId } = render(<BarChart data={sampleData} />);

      expect(getByTestId("recharts-tooltip")).toBeInTheDocument();
    });
  });
});
