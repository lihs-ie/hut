import { Skeleton, SkeletonCircle, SkeletonText } from "@shared/components/atoms/skeleton";
import styles from "./about.module.css";

export const ProfileSkeleton = () => {
  return (
    <div className={styles.profile}>
      <SkeletonCircle size="80px" />
      <div className={styles["profile-info"]}>
        <Skeleton width="10rem" height="1.5rem" variant="text" />
        <Skeleton width="6rem" height="1rem" variant="text" />
        <SkeletonText lines={2} width="100%" />
      </div>
    </div>
  );
};

export const TechStackSkeleton = () => {
  return (
    <div className={styles.section}>
      <div className={styles["section-header"]}>
        <Skeleton width="8rem" height="1.25rem" variant="text" />
      </div>
      <div className={styles.grid}>
        {Array.from({ length: 8 }).map((_, index) => (
          <Skeleton key={index} width="100%" height="60px" variant="rectangle" />
        ))}
      </div>
    </div>
  );
};

export const CareerSkeleton = () => {
  return (
    <div className={styles.section}>
      <div className={styles["section-header"]}>
        <Skeleton width="6rem" height="1.25rem" variant="text" />
      </div>
      <div className={styles.career}>
        {Array.from({ length: 3 }).map((_, index) => (
          <div key={index} className={styles["career-item"]}>
            <Skeleton width="48px" height="48px" variant="rectangle" />
            <div className={styles["career-info"]}>
              <Skeleton width="60%" height="1rem" variant="text" />
              <Skeleton width="40%" height="0.875rem" variant="text" />
            </div>
          </div>
        ))}
      </div>
    </div>
  );
};
