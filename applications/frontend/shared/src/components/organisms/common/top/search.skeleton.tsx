import { Skeleton } from "@shared/components/atoms/skeleton";
import { ContentCardSkeletonList } from "@shared/components/molecules/skeleton";
import styles from "./search.module.css";

type Props = {
  count?: number;
};

export const ContentSectionSkeleton = (props: Props) => {
  const count = props.count ?? 6;

  return (
    <section className={styles.container}>
      <div className={styles.header}>
        <Skeleton width="6rem" height="1.5rem" variant="text" />
      </div>
      <div className={styles.list}>
        <ContentCardSkeletonList count={count} />
      </div>
    </section>
  );
};
