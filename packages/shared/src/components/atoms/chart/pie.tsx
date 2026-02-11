"use client";

import {
  PieChart as RechartsPieChart,
  Pie,
  Cell,
  Tooltip,
  Legend,
  ResponsiveContainer,
} from "recharts";
import { TOOLTIP_STYLE } from "./style";
import styles from "./chart.module.css";

type Props = {
  data: Array<{ label: string; value: number; color?: string }>;
  height?: number;
};

const DEFAULT_COLORS = [
  "var(--chart-1)",
  "var(--chart-2)",
  "var(--chart-3)",
  "var(--chart-4)",
  "var(--chart-5)",
];

export const PieChart = (props: Props) => (
  <div className={styles.container}>
    <ResponsiveContainer width="100%" height={props.height ?? 300}>
      <RechartsPieChart>
        <Pie
          data={props.data}
          dataKey="value"
          nameKey="label"
          cx="50%"
          cy="50%"
          outerRadius={80}
          innerRadius={40}
        >
          {props.data.map((entry, index) => (
            <Cell
              key={entry.label}
              fill={entry.color ?? DEFAULT_COLORS[index % DEFAULT_COLORS.length]}
            />
          ))}
        </Pie>
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
