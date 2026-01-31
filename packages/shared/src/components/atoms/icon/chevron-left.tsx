import styles from "./chevron-left.module.css";

export type Props = {
  className?: string;
};

export const ChevronLeftIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
