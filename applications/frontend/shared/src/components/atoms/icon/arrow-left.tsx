import styles from "./arrow-left.module.css";

export type Props = {
  className?: string;
};

export const ArrowLeftIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
