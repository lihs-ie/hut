import styles from "./logout.module.css";

export type Props = {
  className?: string;
};

export const LogoutIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
