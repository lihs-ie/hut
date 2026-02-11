import styles from "./ranking.module.css";

type Props = {
  rank: number;
  label: string;
  value: string | number;
  subLabel?: string;
  maxValue?: number;
};

const calculateBarWidth = (
  value: string | number,
  maxValue: number | undefined
): string => {
  if (!maxValue || typeof value === "string") return "0%";
  return `${Math.round((value / maxValue) * 100)}%`;
};

export const RankingRow = (props: Props) => (
  <div className={styles.container}>
    <span className={styles.rank}>{props.rank}</span>
    <div className={styles.label}>
      <span className={styles.name}>{props.label}</span>
      {props.subLabel && (
        <span className={styles.sub}>{props.subLabel}</span>
      )}
    </div>
    <div className={styles.bar}>
      <div
        className={styles.fill}
        style={{ width: calculateBarWidth(props.value, props.maxValue) }}
      />
    </div>
    <span className={styles.value}>{props.value}</span>
  </div>
);
