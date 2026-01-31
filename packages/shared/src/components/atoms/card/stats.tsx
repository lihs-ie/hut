import { type ReactNode } from "react";
import styles from "./stats.module.css";

export type Props = {
  title: string;
  value: string | number;
  icon?: ReactNode;
  trend?: {
    value: number;
    label: string;
  };
};

const getTrendClass = (trend: Props["trend"]) => {
  if (!trend) return "";
  if (trend.value > 0) return styles["up"];
  if (trend.value < 0) return styles["down"];
  return styles["neutral"];
};

const getTrendPrefix = (trend: Props["trend"]) => {
  if (!trend) return "";
  if (trend.value > 0) return "+";
  return "";
};

export const StatsCard = (props: Props) => {
  return (
    <div className={styles.container}>
      <div className={styles.header}>
        <span className={styles.title}>{props.title}</span>
        {props.icon && <span className={styles.icon}>{props.icon}</span>}
      </div>
      <div className={styles.value}>{props.value}</div>
      {props.trend && (
        <div className={`${styles.trend} ${getTrendClass(props.trend)}`}>
          <span>
            {getTrendPrefix(props.trend)}
            {props.trend.value}%
          </span>
          <span>{props.trend.label}</span>
        </div>
      )}
    </div>
  );
};
