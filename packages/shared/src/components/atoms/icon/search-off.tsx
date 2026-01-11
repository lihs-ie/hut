import styles from "./search-off.module.css";

export type Props = {
  className?: string;
};

export const SearchOffIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
