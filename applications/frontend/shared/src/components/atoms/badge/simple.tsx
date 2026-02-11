import styles from "./simple.module.css";

export type Props = {
  label: string;
};

export const SimpleBadge = (props: Props) => (
  <span className={styles.container}>{props.label}</span>
);
