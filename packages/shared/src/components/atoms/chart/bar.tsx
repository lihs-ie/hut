"use client";

import {
  BarChart as RechartsBarChart,
  Bar,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  ResponsiveContainer,
} from "recharts";
import { TOOLTIP_STYLE } from "./style";
import styles from "./chart.module.css";

type Props = {
  data: Array<{ label: string; value: number }>;
  height?: number;
  color?: string;
  layout?: "horizontal" | "vertical";
  axisLabel?: { x?: string; y?: string };
};

const AXIS_TICK = { fontSize: 12, fill: "var(--muted-foreground)" };

const buildXAxisLabel = (
  text: string | undefined,
): object | undefined =>
  text
    ? {
        value: text,
        position: "insideBottom",
        offset: -5,
        style: { fontSize: 12, fill: "var(--muted-foreground)" },
      }
    : undefined;

const buildYAxisLabel = (
  text: string | undefined,
): object | undefined =>
  text
    ? {
        value: text,
        angle: -90,
        position: "insideLeft",
        style: {
          fontSize: 12,
          fill: "var(--muted-foreground)",
          textAnchor: "middle",
        },
      }
    : undefined;

export const BarChart = (props: Props) => {
  const layout = props.layout ?? "horizontal";
  const isVertical = layout === "vertical";

  return (
    <div className={styles.container}>
      <ResponsiveContainer width="100%" height={props.height ?? 300}>
        <RechartsBarChart
          data={props.data}
          layout={layout}
          margin={{ top: 5, right: 20, bottom: 25, left: 20 }}
        >
          <CartesianGrid strokeDasharray="3 3" stroke="var(--border)" />
          <XAxis
            {...(isVertical ? { type: "number" as const } : { dataKey: "label" })}
            tick={AXIS_TICK}
            tickLine={false}
            axisLine={{ stroke: "var(--border)" }}
            label={buildXAxisLabel(props.axisLabel?.x)}
          />
          <YAxis
            {...(isVertical ? { type: "category" as const, dataKey: "label", width: 80 } : {})}
            tick={AXIS_TICK}
            tickLine={false}
            axisLine={false}
            label={buildYAxisLabel(props.axisLabel?.y)}
          />
          <Tooltip contentStyle={TOOLTIP_STYLE} />
          <Bar
            dataKey="value"
            fill={props.color ?? "var(--primary)"}
            radius={[4, 4, 0, 0]}
          />
        </RechartsBarChart>
      </ResponsiveContainer>
    </div>
  );
};
