"use client";

import styles from "./toolbar.module.css";

export type Props = {
  onBold: () => void;
  onItalic: () => void;
  onLink: () => void;
  onImage?: () => void;
};

export const EditorToolbar = (props: Props) => {
  return (
    <div className={styles.container}>
      <button
        type="button"
        onClick={props.onBold}
        aria-label="太字"
        className={styles.button}
        title="太字 (Ctrl+B)"
      >
        <strong>B</strong>
      </button>
      <button
        type="button"
        onClick={props.onItalic}
        aria-label="斜体"
        className={styles.button}
        title="斜体 (Ctrl+I)"
      >
        <em>I</em>
      </button>
      <button
        type="button"
        onClick={props.onLink}
        aria-label="リンク挿入"
        className={styles.button}
        title="リンク挿入 (Ctrl+K)"
      >
        リンク
      </button>
      {props.onImage && (
        <button
          type="button"
          onClick={props.onImage}
          aria-label="画像挿入"
          className={styles.button}
          title="画像挿入"
        >
          画像
        </button>
      )}
    </div>
  );
};
