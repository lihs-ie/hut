"use client";

import { useEffect } from "react";
import { AlertTriangleIcon } from "@shared/components/atoms/icon";
import styles from "./error.module.css";

export type ErrorDetail = {
  field?: string;
  description?: string;
};

export type Props = {
  isOpen: boolean;
  onClose: () => void;
  title?: string;
  message: string;
  details?: ErrorDetail[];
  closeText?: string;
};

export const ErrorModal = (props: Props) => {
  useEffect(() => {
    const handleEscape = (event: KeyboardEvent) => {
      if (event.key === "Escape") props.onClose();
    };

    if (props.isOpen) {
      document.addEventListener("keydown", handleEscape);
      document.body.style.overflow = "hidden";
    }

    return () => {
      document.removeEventListener("keydown", handleEscape);
      document.body.style.overflow = "unset";
    };
  }, [props]);

  if (!props.isOpen) return null;

  return (
    <div className={styles.container} onClick={props.onClose}>
      <div
        className={styles.modal}
        onClick={(event) => event.stopPropagation()}
      >
        <div className={styles["icon-wrapper"]}>
          <AlertTriangleIcon className={styles.icon} />
        </div>
        <h2 className={styles.title}>
          {props.title || "エラーが発生しました"}
        </h2>
        <p className={styles.message}>{props.message}</p>
        {props.details && props.details.length > 0 && (
          <ul className={styles.details}>
            {props.details.map((detail, index) => (
              <li key={index} className={styles["detail-item"]}>
                {detail.field && (
                  <span className={styles["detail-field"]}>
                    {detail.field}:
                  </span>
                )}
                <span className={styles["detail-description"]}>
                  {detail.description}
                </span>
              </li>
            ))}
          </ul>
        )}
        <div className={styles.actions}>
          <button onClick={props.onClose} className={styles["close-button"]}>
            {props.closeText || "閉じる"}
          </button>
        </div>
      </div>
    </div>
  );
};
