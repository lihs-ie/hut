import styles from "./tag.module.css";

export type Props = {
  className?: string;
};

export const TagIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
