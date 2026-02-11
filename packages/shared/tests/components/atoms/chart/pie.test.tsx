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

    it("データの数だけCellがレンダリングされる", () => {
      const { getAllByTestId } = render(<PieChart data={sampleData} />);

      const cells = getAllByTestId("recharts-cell");
      expect(cells).toHaveLength(3);
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

    it("カスタムカラーがCellに適用される", () => {
      const dataWithColors = [
        { label: "Desktop", value: 65, color: "#ff0000" },
        { label: "Mobile", value: 30, color: "#00ff00" },
        { label: "Tablet", value: 5, color: "#0000ff" },
      ];

      const { getAllByTestId } = render(
        <PieChart data={dataWithColors} />
      );

      const cells = getAllByTestId("recharts-cell");
      expect(cells[0].dataset.fill).toBe("#ff0000");
      expect(cells[1].dataset.fill).toBe("#00ff00");
      expect(cells[2].dataset.fill).toBe("#0000ff");
    });

    it("カラー未指定の場合はデフォルトのチャートカラーが使用される", () => {
      const { getAllByTestId } = render(<PieChart data={sampleData} />);

      const cells = getAllByTestId("recharts-cell");
      expect(cells[0].dataset.fill).toBe("var(--chart-1)");
      expect(cells[1].dataset.fill).toBe("var(--chart-2)");
      expect(cells[2].dataset.fill).toBe("var(--chart-3)");
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
