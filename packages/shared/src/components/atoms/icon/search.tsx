import styles from "./search.module.css";

export type Props = {
  className?: string;
};

export const SearchIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
