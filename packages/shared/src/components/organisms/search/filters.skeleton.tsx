import { Skeleton } from "@shared/components/atoms/skeleton";
import styles from "./filters.module.css";

export const SearchFilterSkeleton = () => (
  <div className={styles.container}>
    <div className={styles["search-box"]}>
      <div className={styles["search-input-wrapper"]}>
        <Skeleton width="100%" height="3rem" variant="rectangle" />
      </div>
    </div>

    <div className={styles["filter-panel"]}>
      <div className={styles["filter-header"]}>
        <Skeleton width="6rem" height="1.5rem" variant="text" />
      </div>
    </div>
  </div>
);
