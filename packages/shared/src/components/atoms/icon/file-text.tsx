import styles from "./file-text.module.css";

export type Props = {
  className?: string;
};

export const FileTextIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
