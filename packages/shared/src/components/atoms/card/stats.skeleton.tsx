import { Skeleton } from "@shared/components/atoms/skeleton";
import styles from "./stats.module.css";

type Props = {
  count?: number;
};

export const StatsCardSkeleton = (props: Props) => (
  <>
    {Array.from({ length: props.count ?? 4 }).map((_, index) => (
      <div key={index} className={styles.container}>
        <div className={styles.header}>
          <Skeleton variant="text" width="60%" />
        </div>
        <Skeleton variant="text" width="40%" height="2rem" />
      </div>
    ))}
  </>
);
