import { Skeleton } from "@shared/components/atoms/skeleton";
import styles from "./header.module.css";

export const AdminSearchHeaderSkeleton = () => (
  <div className={styles.container}>
    <div className={styles.header}>
      <Skeleton width="8rem" height="1.75rem" variant="text" />
      <Skeleton width="6rem" height="2.25rem" variant="rectangle" />
    </div>
    <div className={styles.input}>
      <Skeleton width="100%" height="2.5rem" variant="rectangle" />
    </div>
    <div className={styles.selects}>
      <Skeleton width="8rem" height="2.25rem" variant="rectangle" />
      <Skeleton width="6rem" height="2.25rem" variant="rectangle" />
    </div>
    <div className={styles.tags}>
      <Skeleton width="100%" height="5rem" variant="rectangle" />
    </div>
  </div>
);
