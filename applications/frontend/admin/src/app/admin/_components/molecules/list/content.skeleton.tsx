import { ContentCardSkeletonList } from "@shared/components/molecules/skeleton";
import styles from "./content.module.css";

type Props = {
  count?: number;
};

export const AdminContentListSkeleton = (props: Props) => (
  <div className={styles.container}>
    <ContentCardSkeletonList count={props.count ?? 6} />
  </div>
);
