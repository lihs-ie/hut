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
import type { LabelProps } from "recharts";
import { TOOLTIP_STYLE } from "./style";
import styles from "./chart.module.css";

type Props = {
  data: Array<{ label: string; value: number }>;
  height?: number;
  color?: string;
  axisLabel?: { x?: string; y?: string };
};

const AXIS_TICK: React.SVGProps<SVGTextElement> = {
  fontSize: 12,
  fill: "var(--muted-foreground)",
};

const LABEL_STYLE: React.CSSProperties = {
  fontSize: 12,
  fill: "var(--muted-foreground)",
};

const buildAxisLabel = (
  text: string | undefined,
  placement: Partial<LabelProps>,
): LabelProps | undefined =>
  text
    ? { value: text, style: LABEL_STYLE, ...placement }
    : undefined;

const X_AXIS_PLACEMENT: Partial<LabelProps> = {
  position: "insideBottom",
  offset: -5,
};

const Y_AXIS_PLACEMENT: Partial<LabelProps> = {
  angle: -90,
  position: "insideLeft",
  style: { ...LABEL_STYLE, textAnchor: "middle" },
};

export const BarChart = (props: Props) => (
  <div className={styles.container}>
    <ResponsiveContainer width="100%" height={props.height ?? 300}>
      <RechartsBarChart
        data={props.data}
        margin={{ top: 5, right: 20, bottom: 25, left: 20 }}
      >
        <CartesianGrid strokeDasharray="3 3" stroke="var(--border)" />
        <XAxis
          dataKey="label"
          tick={AXIS_TICK}
          tickLine={false}
          axisLine={{ stroke: "var(--border)" }}
          label={buildAxisLabel(props.axisLabel?.x, X_AXIS_PLACEMENT)}
        />
        <YAxis
          tick={AXIS_TICK}
          tickLine={false}
          axisLine={false}
          label={buildAxisLabel(props.axisLabel?.y, Y_AXIS_PLACEMENT)}
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
