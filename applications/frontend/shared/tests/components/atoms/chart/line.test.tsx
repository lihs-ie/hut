/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render } from "@testing-library/react";
import { LineChart } from "@shared/components/atoms/chart/line";

vi.mock("recharts", () => {
  const MockResponsiveContainer = (props: Record<string, unknown>) => (
    <div data-testid="responsive-container" data-height={String(props.height)}>
      {props.children as React.ReactNode}
    </div>
  );
  const MockLineChart = (props: Record<string, unknown>) => (
    <div data-testid="recharts-line-chart" data-data={JSON.stringify(props.data)}>
      {props.children as React.ReactNode}
    </div>
  );
  const MockLine = (props: Record<string, unknown>) => (
    <div data-testid="recharts-line" data-stroke={String(props.stroke)} data-datakey={String(props.dataKey)} />
  );
  const MockXAxis = (props: Record<string, unknown>) => (
    <div data-testid="recharts-xaxis" data-datakey={String(props.dataKey)} />
  );
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

describe("components/atoms/chart/LineChart", () => {
  const sampleData = [
    { label: "1/1", value: 100 },
    { label: "1/2", value: 200 },
    { label: "1/3", value: 150 },
  ];

  describe("表示", () => {
    it("ResponsiveContainerがレンダリングされる", () => {
      const { getByTestId } = render(<LineChart data={sampleData} />);

      expect(getByTestId("responsive-container")).toBeInTheDocument();
    });

    it("RechartsのLineChartがレンダリングされる", () => {
      const { getByTestId } = render(<LineChart data={sampleData} />);

      expect(getByTestId("recharts-line-chart")).toBeInTheDocument();
    });

    it("データがLineChartに渡される", () => {
      const { getByTestId } = render(<LineChart data={sampleData} />);

      const chart = getByTestId("recharts-line-chart");
      expect(chart.dataset.data).toBe(JSON.stringify(sampleData));
    });

    it("XAxisにlabelがdataKeyとして設定される", () => {
      const { getByTestId } = render(<LineChart data={sampleData} />);

      const xAxis = getByTestId("recharts-xaxis");
      expect(xAxis.dataset.datakey).toBe("label");
    });

    it("LineにvalueがdataKeyとして設定される", () => {
      const { getByTestId } = render(<LineChart data={sampleData} />);

      const line = getByTestId("recharts-line");
      expect(line.dataset.datakey).toBe("value");
    });
  });

  describe("プロパティ", () => {
    it("デフォルトの高さは300", () => {
      const { getByTestId } = render(<LineChart data={sampleData} />);

      const container = getByTestId("responsive-container");
      expect(container.dataset.height).toBe("300");
    });

    it("カスタムの高さが設定できる", () => {
      const { getByTestId } = render(
        <LineChart data={sampleData} height={400} />
      );

      const container = getByTestId("responsive-container");
      expect(container.dataset.height).toBe("400");
    });

    it("デフォルトのカラーはvar(--primary)", () => {
      const { getByTestId } = render(<LineChart data={sampleData} />);

      const line = getByTestId("recharts-line");
      expect(line.dataset.stroke).toBe("var(--primary)");
    });

    it("カスタムカラーが設定できる", () => {
      const { getByTestId } = render(
        <LineChart data={sampleData} color="var(--accent)" />
      );

      const line = getByTestId("recharts-line");
      expect(line.dataset.stroke).toBe("var(--accent)");
    });
  });

  describe("構造", () => {
    it("ルート要素にcontainerクラスが適用される", () => {
      const { container } = render(<LineChart data={sampleData} />);

      const root = container.firstElementChild;
      expect(root?.className).toContain("container");
    });

    it("CartesianGridが含まれる", () => {
      const { getByTestId } = render(<LineChart data={sampleData} />);

      expect(getByTestId("recharts-grid")).toBeInTheDocument();
    });

    it("Tooltipが含まれる", () => {
      const { getByTestId } = render(<LineChart data={sampleData} />);

      expect(getByTestId("recharts-tooltip")).toBeInTheDocument();
    });

    it("YAxisが含まれる", () => {
      const { getByTestId } = render(<LineChart data={sampleData} />);

      expect(getByTestId("recharts-yaxis")).toBeInTheDocument();
    });
  });
});
