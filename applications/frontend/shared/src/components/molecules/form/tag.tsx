"use client";

import Image from "next/image";
import styles from "./tag.module.css";

export type Props = {
  name: string;
  onNameChange: (name: string) => void;
  logoPreview: string | null;
  onLogoChange: (src: string) => void;
};

export const TagForm = (props: Props) => (
  <div className={styles.container}>
    <div className={styles["form-group"]}>
      <label htmlFor="name" className={styles.label}>
        タグ名 <span className={styles.required}>*</span>
      </label>
      <input
        id="name"
        type="text"
        value={props.name}
        onChange={(event) => props.onNameChange(event.target.value)}
        placeholder="例: Next.js"
        required
        className={styles.input}
      />
    </div>

    <div className={styles["form-group"]}>
      <label htmlFor="logo" className={styles.label}>
        ロゴ画像URL <span className={styles.required}>*</span>
      </label>
      <input
        id="logo"
        type="url"
        value={props.logoPreview || ""}
        onChange={(event) => props.onLogoChange(event.target.value)}
        placeholder="https://example.com/logo.png"
        required
        className={styles.input}
      />
      {props.logoPreview && (
        <div className={styles["logo-preview"]}>
          <Image
            src={props.logoPreview}
            alt="Logo preview"
            width={80}
            height={80}
            className={styles["logo-image"]}
          />
        </div>
      )}
      <p className={styles.hint}>
        画像のURLを入力してください（SVG、PNG、JPG対応）
      </p>
    </div>
  </div>
);
