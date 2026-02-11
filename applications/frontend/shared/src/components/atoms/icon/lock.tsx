import styles from "./lock.module.css";

export type Props = {
  className?: string;
};

export const LockIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
