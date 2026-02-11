import { ChartSkeleton } from "@shared/components/atoms/chart/skeleton";
import styles from "./grid.module.css";

export const PageViewTrendSkeleton = () => (
  <div className={styles.container}>
    <ChartSkeleton />
    <ChartSkeleton />
  </div>
);
