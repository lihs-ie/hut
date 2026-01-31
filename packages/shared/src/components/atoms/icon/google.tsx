import styles from "./google.module.css";

export type Props = {
  className?: string;
};

export const GoogleIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
