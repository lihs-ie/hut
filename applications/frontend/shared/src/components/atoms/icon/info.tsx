import styles from "./info.module.css";

export type Props = {
  className?: string;
};

export const InfoIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
