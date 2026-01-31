import styles from "./sun.module.css";

export type Props = {
  className?: string;
};

export const SunIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
