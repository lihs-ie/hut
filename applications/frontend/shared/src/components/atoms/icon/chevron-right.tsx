import styles from "./chevron-right.module.css";

export type Props = {
  className?: string;
};

export const ChevronRightIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
