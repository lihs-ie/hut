import { ChartSkeleton } from "@shared/components/atoms/chart/skeleton";
import styles from "./search.module.css";

export const SearchAnalyticsSectionSkeleton = () => (
  <div className={styles.container}>
    <ChartSkeleton />
    <div className={styles.rankings}>
      <ChartSkeleton />
      <ChartSkeleton />
    </div>
  </div>
);
