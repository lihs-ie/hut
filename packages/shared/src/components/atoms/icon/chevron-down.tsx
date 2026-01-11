import styles from "./chevron-down.module.css";

export type Props = {
  className?: string;
};

export const ChevronDownIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
