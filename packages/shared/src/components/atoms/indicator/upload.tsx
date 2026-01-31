import styles from "./upload.module.css";

type Props = {
  progress: number;
  status: "compressing" | "uploading" | "completed" | "failed";
};

export const UploadIndicator = (props: Props) => (
  <div className={styles.container}>
    <div
      className={styles.bar}
      data-status={props.status}
      style={{ width: `${props.progress}%` }}
    />
  </div>
);
