import { Skeleton } from "@shared/components/atoms/skeleton";
import styles from "./tag-card.module.css";

type Props = {
  className?: string;
};

export const TagCardSkeleton = (props: Props) => {
  return (
    <div className={`${styles.container} ${props.className ?? ""}`}>
      <Skeleton width="48px" height="48px" variant="rectangle" />
      <div className={styles.info}>
        <Skeleton width="60%" height="1.125rem" variant="text" />
        <Skeleton width="5rem" height="0.75rem" variant="text" />
      </div>
    </div>
  );
};

type TagCardSkeletonListProps = {
  count?: number;
  className?: string;
};

export const TagCardSkeletonList = (props: TagCardSkeletonListProps) => {
  const count = props.count ?? 6;

  return (
    <>
      {Array.from({ length: count }).map((_, index) => (
        <TagCardSkeleton key={index} className={props.className} />
      ))}
    </>
  );
};
