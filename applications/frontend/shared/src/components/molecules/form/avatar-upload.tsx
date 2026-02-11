import type { ChangeEvent } from "react";
import { UploadIcon } from "@shared/components/atoms/icon/upload";
import styles from "./avatar-upload.module.css";
import { CircleImage } from "../image/circle";

export type Props = {
  avatarUrl: string;
  displayName: string;
  onAvatarChange: (file: File) => void;
};

export const AvatarUpload = (props: Props) => {
  const handleChange = (event: ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0];
    if (file) {
      props.onAvatarChange(file);
    }
  };

  return (
    <div className={styles.container}>
      <div className={styles.avatar}>
        <CircleImage src={props.avatarUrl} alt={props.displayName} />
      </div>
      <label className={styles.button}>
        <UploadIcon className={styles.icon} />
        変更する
        <input
          type="file"
          accept="image/*"
          onChange={handleChange}
          className={styles.input}
        />
      </label>
    </div>
  );
};
