import styles from "./moon.module.css";

export type Props = {
  className?: string;
};

export const MoonIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
