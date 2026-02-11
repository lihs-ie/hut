import styles from "./save.module.css";

export type Props = {
  className?: string;
};

export const SaveIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
