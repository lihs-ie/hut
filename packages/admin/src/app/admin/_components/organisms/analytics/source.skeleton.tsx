import { ChartSkeleton } from "@shared/components/atoms/chart/skeleton";
import styles from "./grid.module.css";

export const AccessSourceSectionSkeleton = () => (
  <div className={styles.container}>
    <ChartSkeleton />
    <ChartSkeleton />
  </div>
);
