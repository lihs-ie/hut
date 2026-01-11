import { Skeleton } from "@shared/components/atoms/skeleton";
import { TagCardSkeletonList } from "@shared/components/molecules/skeleton";
import styles from "./list.module.css";

type Props = {
  count?: number;
};

export const TagListSkeleton = (props: Props) => {
  const count = props.count ?? 6;

  return (
    <div className={styles.container}>
      <div className={styles["search-bar"]}>
        <Skeleton width="100%" height="2.5rem" variant="rectangle" />
      </div>
      <div className={styles.grid}>
        <TagCardSkeletonList count={count} />
      </div>
    </div>
  );
};
