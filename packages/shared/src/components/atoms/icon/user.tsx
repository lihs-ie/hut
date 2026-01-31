import styles from "./user.module.css";

export type Props = {
  className?: string;
};

export const UserIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
