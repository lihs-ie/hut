import styles from "./check-circle.module.css";

export type Props = {
  className?: string;
};

export const CheckCircleIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
