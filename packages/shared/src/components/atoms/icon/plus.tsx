import styles from "./plus.module.css";

export type Props = {
  className?: string;
};

export const PlusIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
