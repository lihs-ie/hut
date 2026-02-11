"use client";

import { useEffect, type ComponentType } from "react";
import { AlertTriangleIcon } from "@shared/components/atoms/icon/alert-triangle";
import { InfoIcon } from "@shared/components/atoms/icon/info";
import { CheckCircleIcon } from "@shared/components/atoms/icon/check-circle";
import { XCircleIcon } from "@shared/components/atoms/icon/x-circle";
import styles from "./confirm.module.css";

export type ConfirmModalType = "info" | "warning" | "success" | "error";

export type Props = {
  isOpen: boolean;
  onClose: () => void;
  onConfirm: () => void | Promise<void>;
  title: string;
  message: string;
  confirmText?: string;
  cancelText?: string;
  type?: ConfirmModalType;
};

const iconMap: Record<
  ConfirmModalType,
  ComponentType<{ className?: string }>
> = {
  info: InfoIcon,
  warning: AlertTriangleIcon,
  success: CheckCircleIcon,
  error: XCircleIcon,
};

export const ConfirmModal = (props: Props) => {
  const Icon = iconMap[props.type || "info"];

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

  if (!props.isOpen) return <></>;

  return (
    <div className={styles.container} onClick={props.onClose}>
      <div className={styles.modal} onClick={(e) => e.stopPropagation()}>
        <div
          className={`${styles.iconWrapper} ${styles[props.type || "info"]}`}
        >
          <Icon className={styles.icon} />
        </div>
        <h2 className={styles.title}>{props.title}</h2>
        <p className={styles.message}>{props.message}</p>
        <div className={styles.actions}>
          <button
            type="button"
            onClick={props.onClose}
            className={styles.cancelButton}
          >
            {props.cancelText || "キャンセル"}
          </button>
          <button
            type="button"
            onClick={async () => {
              await props.onConfirm();
            }}
            className={styles.confirmButton}
          >
            {props.confirmText || "確認"}
          </button>
        </div>
      </div>
    </div>
  );
};
