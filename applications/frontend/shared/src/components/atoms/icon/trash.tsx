import styles from "./trash.module.css";

export type Props = {
  className?: string;
};

export const TrashIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);

export const Trash2Icon = TrashIcon;
