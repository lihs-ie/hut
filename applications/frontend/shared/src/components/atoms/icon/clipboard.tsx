import styles from "./clipboard.module.css";

export type Props = {
  className?: string;
};

export const ClipboardIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
