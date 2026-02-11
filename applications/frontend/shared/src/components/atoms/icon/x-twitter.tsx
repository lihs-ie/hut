import styles from "./x-twitter.module.css";

export type Props = {
  className?: string;
};

export const XTwitterIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
