import styles from "./filter.module.css";

export type Props = {
  className?: string;
};

export const FilterIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
