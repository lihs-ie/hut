import styles from "./pen.module.css";

export type Props = {
  className?: string;
};

export const PenIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
