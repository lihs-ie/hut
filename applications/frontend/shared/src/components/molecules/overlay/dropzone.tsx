import { UploadIcon } from "@shared/components/atoms/icon/upload";
import styles from "./dropzone.module.css";

type Props = {
  isActive: boolean;
};

export const DropzoneOverlay = (props: Props) => {
  if (!props.isActive) {
    return null;
  }

  return (
    <div className={styles.container}>
      <div className={styles.content}>
        <UploadIcon className={styles.icon} />
        <p className={styles.text}>画像をドロップしてアップロード</p>
      </div>
    </div>
  );
};
