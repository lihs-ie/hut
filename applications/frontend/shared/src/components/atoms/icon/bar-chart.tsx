import styles from "./bar-chart.module.css";

export type Props = {
  className?: string;
};

export const BarChartIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
