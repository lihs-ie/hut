"use client";

import dynamic from "next/dynamic";
import { SectionHeader } from "@shared/components/atoms/text/section";
import { ChartSkeleton } from "@shared/components/atoms/chart/skeleton";
import styles from "./panel.module.css";

const PieChart = dynamic(
  () =>
    import("@shared/components/atoms/chart/pie").then((module) => ({
      default: module.PieChart,
    })),
  { loading: () => <ChartSkeleton /> },
);

type Props = {
  title: string;
  description?: string;
  data: Array<{ label: string; value: number; color?: string }>;
  height?: number;
};

export const PieChartPanel = (props: Props) => (
  <div className={styles.container}>
    <SectionHeader title={props.title} description={props.description} />
    <PieChart data={props.data} height={props.height} />
  </div>
);
