import { StatsCardSkeleton } from "@shared/components/atoms/card/stats.skeleton";
import styles from "./summary.module.css";

export const AnalyticsSummarySkeleton = () => (
  <div className={styles.container}>
    <StatsCardSkeleton count={4} />
  </div>
);
