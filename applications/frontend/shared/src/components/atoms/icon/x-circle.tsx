import styles from "./x-circle.module.css";

export type Props = {
  className?: string;
};

export const XCircleIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
