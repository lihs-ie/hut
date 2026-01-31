import styles from "./message.module.css";

export type Props = {
  className?: string;
};

export const MessageIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);

export const MessageSquareIcon = MessageIcon;
