import styles from "./clock.module.css";

export type Props = {
  className?: string;
};

export const ClockIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
