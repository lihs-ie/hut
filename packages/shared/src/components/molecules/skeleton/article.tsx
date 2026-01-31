import { Skeleton, SkeletonText } from "@shared/components/atoms/skeleton";
import styles from "./article.module.css";

export const ArticleTitleSkeleton = () => {
  return (
    <div className={styles.title}>
      <Skeleton width="60%" height="2rem" variant="text" />
      <Skeleton width="10rem" height="1rem" variant="text" />
    </div>
  );
};

export const ArticleContentSkeleton = () => {
  return (
    <div className={styles.content}>
      <SkeletonText lines={4} width="100%" />
      <Skeleton width="100%" height="200px" variant="rectangle" />
      <SkeletonText lines={6} width="100%" />
      <SkeletonText lines={3} width="100%" />
    </div>
  );
};

export const ArticleSidebarSkeleton = () => {
  return (
    <div className={styles.sidebar}>
      <div className={styles["sidebar-title"]}>
        <Skeleton width="5rem" height="1rem" variant="text" />
      </div>
      <Skeleton width="80%" height="0.875rem" variant="text" />
      <Skeleton width="70%" height="0.875rem" variant="text" />
      <Skeleton width="90%" height="0.875rem" variant="text" />
      <Skeleton width="60%" height="0.875rem" variant="text" />
    </div>
  );
};
