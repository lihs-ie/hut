import { ContentCardSkeletonList } from "@shared/components/molecules/skeleton";
import { Skeleton } from "@shared/components/atoms/skeleton";
import styles from "./result.module.css";

type Props = {
  count?: number;
};

export const SearchResultSkeleton = (props: Props) => {
  const count = props.count ?? 6;

  return (
    <div className={styles.container}>
      <div className={styles.header}>
        <Skeleton width="8rem" height="1rem" variant="text" />
      </div>
      <div className={styles.contents}>
        <ContentCardSkeletonList count={count} />
      </div>
    </div>
  );
};
