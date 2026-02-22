"use client";

import {
  PieChart as RechartsPieChart,
  Pie,
  Tooltip,
  Legend,
  ResponsiveContainer,
} from "recharts";
import { TOOLTIP_STYLE } from "./style";
import styles from "./chart.module.css";

type PieDataEntry = { label: string; value: number; color?: string };

type Props = {
  data: Array<PieDataEntry>;
  height?: number;
};

const DEFAULT_COLORS = [
  "var(--chart-1)",
  "var(--chart-2)",
  "var(--chart-3)",
  "var(--chart-4)",
  "var(--chart-5)",
];

type PieDataEntryWithFill = { label: string; value: number; fill: string };

const withFill = (data: Array<PieDataEntry>): Array<PieDataEntryWithFill> =>
  data.map((entry, index) => ({
    label: entry.label,
    value: entry.value,
    fill: entry.color ?? DEFAULT_COLORS[index % DEFAULT_COLORS.length],
  }));

export const PieChart = (props: Props) => (
  <div className={styles.container}>
    <ResponsiveContainer width="100%" height={props.height ?? 300}>
      <RechartsPieChart>
        <Pie
          data={withFill(props.data)}
          dataKey="value"
          nameKey="label"
          cx="50%"
          cy="50%"
          outerRadius={80}
          innerRadius={40}
        />
        <Tooltip contentStyle={TOOLTIP_STYLE} />
        <Legend
          verticalAlign="bottom"
          iconType="circle"
          wrapperStyle={{ fontSize: "0.8125rem" }}
        />
      </RechartsPieChart>
    </ResponsiveContainer>
  </div>
);
