"use client";

import dynamic from "next/dynamic";
import { SectionHeader } from "@shared/components/atoms/text/section";
import { ChartSkeleton } from "@shared/components/atoms/chart/skeleton";
import styles from "./panel.module.css";

const BarChart = dynamic(
  () =>
    import("@shared/components/atoms/chart/bar").then((module) => ({
      default: module.BarChart,
    })),
  { loading: () => <ChartSkeleton /> },
);

type Props = {
  title: string;
  description?: string;
  data: Array<{ label: string; value: number }>;
  color?: string;
  height?: number;
  axisLabel?: { x?: string; y?: string };
};

export const BarChartPanel = (props: Props) => (
  <div className={styles.container}>
    <SectionHeader title={props.title} description={props.description} />
    <BarChart
      data={props.data}
      color={props.color}
      height={props.height}
      axisLabel={props.axisLabel}
    />
  </div>
);
