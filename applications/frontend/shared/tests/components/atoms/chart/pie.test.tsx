/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { render } from "@testing-library/react";
import { PieChart } from "@shared/components/atoms/chart/pie";

vi.mock("recharts", () => {
  const MockResponsiveContainer = (props: Record<string, unknown>) => (
    <div data-testid="responsive-container" data-height={String(props.height)}>
      {props.children as React.ReactNode}
    </div>
  );
  const MockPieChart = (props: Record<string, unknown>) => (
    <div data-testid="recharts-pie-chart">
      {props.children as React.ReactNode}
    </div>
  );
  const MockPie = (props: Record<string, unknown>) => (
    <div data-testid="recharts-pie" data-datakey={String(props.dataKey)} data-data={JSON.stringify(props.data)}>
      {props.children as React.ReactNode}
    </div>
  );
  const MockTooltip = () => <div data-testid="recharts-tooltip" />;
  const MockLegend = () => <div data-testid="recharts-legend" />;

  return {
    ResponsiveContainer: MockResponsiveContainer,
    PieChart: MockPieChart,
    Pie: MockPie,
    Tooltip: MockTooltip,
    Legend: MockLegend,
  };
});

describe("components/atoms/chart/PieChart", () => {
  const sampleData = [
    { label: "Desktop", value: 65 },
    { label: "Mobile", value: 30 },
    { label: "Tablet", value: 5 },
  ];

  describe("表示", () => {
    it("ResponsiveContainerがレンダリングされる", () => {
      const { getByTestId } = render(<PieChart data={sampleData} />);

      expect(getByTestId("responsive-container")).toBeInTheDocument();
    });

    it("RechartsのPieChartがレンダリングされる", () => {
      const { getByTestId } = render(<PieChart data={sampleData} />);

      expect(getByTestId("recharts-pie-chart")).toBeInTheDocument();
    });

    it("Pieコンポーネントがレンダリングされる", () => {
      const { getByTestId } = render(<PieChart data={sampleData} />);

      const pie = getByTestId("recharts-pie");
      expect(pie.dataset.datakey).toBe("value");
    });

    it("データの各要素にfillが設定される", () => {
      const { getByTestId } = render(<PieChart data={sampleData} />);

      const pie = getByTestId("recharts-pie");
      const parsedData = JSON.parse(pie.dataset.data ?? "[]") as Array<{ fill: string }>;
      expect(parsedData).toHaveLength(3);
      expect(parsedData[0].fill).toBe("var(--chart-1)");
      expect(parsedData[1].fill).toBe("var(--chart-2)");
      expect(parsedData[2].fill).toBe("var(--chart-3)");
    });
  });

  describe("プロパティ", () => {
    it("デフォルトの高さは300", () => {
      const { getByTestId } = render(<PieChart data={sampleData} />);

      const container = getByTestId("responsive-container");
      expect(container.dataset.height).toBe("300");
    });

    it("カスタムの高さが設定できる", () => {
      const { getByTestId } = render(
        <PieChart data={sampleData} height={400} />
      );

      const container = getByTestId("responsive-container");
      expect(container.dataset.height).toBe("400");
    });

    it("カスタムカラーがデータのfillに適用される", () => {
      const dataWithColors = [
        { label: "Desktop", value: 65, color: "#ff0000" },
        { label: "Mobile", value: 30, color: "#00ff00" },
        { label: "Tablet", value: 5, color: "#0000ff" },
      ];

      const { getByTestId } = render(
        <PieChart data={dataWithColors} />
      );

      const pie = getByTestId("recharts-pie");
      const parsedData = JSON.parse(pie.dataset.data ?? "[]") as Array<{ fill: string }>;
      expect(parsedData[0].fill).toBe("#ff0000");
      expect(parsedData[1].fill).toBe("#00ff00");
      expect(parsedData[2].fill).toBe("#0000ff");
    });

    it("カラー未指定の場合はデフォルトのチャートカラーが使用される", () => {
      const { getByTestId } = render(<PieChart data={sampleData} />);

      const pie = getByTestId("recharts-pie");
      const parsedData = JSON.parse(pie.dataset.data ?? "[]") as Array<{ fill: string }>;
      expect(parsedData[0].fill).toBe("var(--chart-1)");
      expect(parsedData[1].fill).toBe("var(--chart-2)");
      expect(parsedData[2].fill).toBe("var(--chart-3)");
    });
  });

  describe("構造", () => {
    it("ルート要素にcontainerクラスが適用される", () => {
      const { container } = render(<PieChart data={sampleData} />);

      const root = container.firstElementChild;
      expect(root?.className).toContain("container");
    });

    it("Tooltipが含まれる", () => {
      const { getByTestId } = render(<PieChart data={sampleData} />);

      expect(getByTestId("recharts-tooltip")).toBeInTheDocument();
    });

    it("Legendが含まれる", () => {
      const { getByTestId } = render(<PieChart data={sampleData} />);

      expect(getByTestId("recharts-legend")).toBeInTheDocument();
    });
  });
});
