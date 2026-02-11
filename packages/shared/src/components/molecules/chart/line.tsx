"use client";

import dynamic from "next/dynamic";
import { SectionHeader } from "@shared/components/atoms/text/section";
import { ChartSkeleton } from "@shared/components/atoms/chart/skeleton";
import styles from "./panel.module.css";

const LineChart = dynamic(
  () =>
    import("@shared/components/atoms/chart/line").then((module) => ({
      default: module.LineChart,
    })),
  { loading: () => <ChartSkeleton /> },
);

type Props = {
  title: string;
  description?: string;
  data: Array<{ label: string; value: number }>;
  color?: string;
  height?: number;
};

export const LineChartPanel = (props: Props) => (
  <div className={styles.container}>
    <SectionHeader title={props.title} description={props.description} />
    <LineChart
      data={props.data}
      color={props.color}
      height={props.height}
    />
  </div>
);
