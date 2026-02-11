import styles from "./eye.module.css";

export type Props = {
  className?: string;
};

export const EyeIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
