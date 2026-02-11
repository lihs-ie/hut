import styles from "./skeleton.module.css";

type Props = {
  height?: number;
};

export const ChartSkeleton = (props: Props) => (
  <div
    className={styles.container}
    style={{ height: `${props.height ?? 300}px` }}
  >
    <div className={styles.shimmer} />
  </div>
);
