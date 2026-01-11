import styles from "./upload.module.css";

export type Props = {
  className?: string;
};

export const UploadIcon = (props: Props) => (
  <span
    className={`${styles.container} ${props.className ?? ""}`}
    role="img"
    aria-hidden="true"
  />
);
