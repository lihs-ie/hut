import { Skeleton } from "@shared/components/atoms/skeleton";
import styles from "./content-card.module.css";

type Props = {
  className?: string;
};

export const ContentCardSkeleton = (props: Props) => {
  return (
    <div className={`${styles.container} ${props.className ?? ""}`}>
      <div className={styles.header}>
        <Skeleton width="3.5rem" height="1.25rem" variant="rectangle" />
        <Skeleton width="5rem" height="0.875rem" variant="text" />
      </div>

      <div className={styles.body}>
        <Skeleton width="100%" height="1.25rem" variant="text" />
        <Skeleton width="70%" height="1.25rem" variant="text" />
      </div>

      <div className={styles.tags}>
        <Skeleton width="3rem" height="0.875rem" variant="text" />
        <Skeleton width="4rem" height="0.875rem" variant="text" />
        <Skeleton width="3.5rem" height="0.875rem" variant="text" />
      </div>
    </div>
  );
};

type ContentCardSkeletonListProps = {
  count?: number;
  className?: string;
};

export const ContentCardSkeletonList = (props: ContentCardSkeletonListProps) => {
  const count = props.count ?? 6;

  return (
    <>
      {Array.from({ length: count }).map((_, index) => (
        <ContentCardSkeleton key={index} className={props.className} />
      ))}
    </>
  );
};
