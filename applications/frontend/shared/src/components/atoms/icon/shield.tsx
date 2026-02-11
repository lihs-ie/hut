import styles from "./shield.module.css";

export type Props = {
  className?: string;
};

export const ShieldIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
