"use client";

import { createContext, useContext, useState, useCallback } from "react";
import styles from "./index.module.css";

type ToastItem = {
  id: string;
  message: string;
  isExiting: boolean;
};

type ToastContextValue = {
  showToast: (message: string) => void;
};

const ToastContext = createContext<ToastContextValue | null>(null);

const TOAST_DURATION_MS = 3000;
const TOAST_EXIT_ANIMATION_MS = 200;

type Props = {
  children: React.ReactNode;
};

export const ToastProvider = (props: Props) => {
  const [toasts, setToasts] = useState<ToastItem[]>([]);

  const showToast = useCallback((message: string) => {
    const id = `${Date.now()}-${Math.random().toString(36).slice(2)}`;
    setToasts((previous) => [...previous, { id, message, isExiting: false }]);

    setTimeout(() => {
      setToasts((previous) =>
        previous.map((toast) =>
          toast.id === id ? { ...toast, isExiting: true } : toast,
        ),
      );

      setTimeout(() => {
        setToasts((previous) => previous.filter((toast) => toast.id !== id));
      }, TOAST_EXIT_ANIMATION_MS);
    }, TOAST_DURATION_MS);
  }, []);

  return (
    <ToastContext.Provider value={{ showToast }}>
      {props.children}
      {toasts.length > 0 && (
        <div className={styles.container} role="status" aria-live="polite">
          {toasts.map((toast) => (
            <div
              key={toast.id}
              className={`${styles.toast}${toast.isExiting ? ` ${styles.exiting}` : ""}`}
            >
              {toast.message}
            </div>
          ))}
        </div>
      )}
    </ToastContext.Provider>
  );
};

export const useToast = (): ToastContextValue => {
  const context = useContext(ToastContext);
  if (!context) {
    throw new Error("useToast は ToastProvider の配下で使用してください");
  }
  return context;
};
