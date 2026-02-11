import styles from "./cross.module.css";

export type Props = {
  className?: string;
};

export const CrossIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);

export const XIcon = CrossIcon;
