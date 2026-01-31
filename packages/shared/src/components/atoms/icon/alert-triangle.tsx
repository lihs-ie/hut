import styles from "./alert-triangle.module.css";

export type Props = {
  className?: string;
};

export const AlertTriangleIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
