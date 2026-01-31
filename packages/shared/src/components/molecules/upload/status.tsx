import { UploadIndicator } from "@shared/components/atoms/indicator/upload";
import { CrossIcon } from "@shared/components/atoms/icon/cross";
import type { UploadState } from "@shared/hooks/useImageUpload";
import styles from "./status.module.css";

type Props = {
  uploads: UploadState[];
  onCancel: (id: string) => void;
  onClear: () => void;
};

const STATUS_LABELS: Record<UploadState["status"], string> = {
  compressing: "圧縮中...",
  uploading: "アップロード中...",
  completed: "完了",
  failed: "失敗",
};

export const UploadStatus = (props: Props) => {
  if (props.uploads.length === 0) {
    return null;
  }

  const hasCompleted = props.uploads.some(
    (upload) => upload.status === "completed" || upload.status === "failed",
  );

  return (
    <div className={styles.container}>
      <div className={styles.header}>
        <span className={styles.title}>アップロード</span>
        {hasCompleted && (
          <button
            type="button"
            onClick={props.onClear}
            className={styles.clear}
          >
            クリア
          </button>
        )}
      </div>
      <ul className={styles.list}>
        {props.uploads.map((upload) => (
          <li key={upload.id} className={styles.item}>
            <div className={styles.info}>
              <span className={styles.filename}>{upload.fileName}</span>
              <span className={styles.status} data-status={upload.status}>
                {upload.error || STATUS_LABELS[upload.status]}
              </span>
            </div>
            <UploadIndicator
              progress={upload.progress}
              status={upload.status}
            />
            {(upload.status === "compressing" ||
              upload.status === "uploading") && (
              <button
                type="button"
                onClick={() => props.onCancel(upload.id)}
                className={styles.cancel}
                aria-label="キャンセル"
              >
                <CrossIcon />
              </button>
            )}
          </li>
        ))}
      </ul>
    </div>
  );
};
