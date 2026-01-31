import styles from "./mail.module.css";

export type Props = {
  className?: string;
};

export const MailIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
