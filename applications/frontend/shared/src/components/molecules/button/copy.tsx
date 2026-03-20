"use client";

import { useState, useRef, useCallback, useEffect } from "react";
import styles from "./copy.module.css";

export type Props = {
  text: string;
};

export const CopyButton = (props: Props) => {
  const [copied, setCopied] = useState(false);
  const timerReference = useRef<ReturnType<typeof setTimeout> | null>(null);

  useEffect(() => {
    return () => {
      if (timerReference.current !== null) {
        clearTimeout(timerReference.current);
      }
    };
  }, []);

  const handleClick = useCallback(async () => {
    try {
      await navigator.clipboard.writeText(props.text);
      setCopied(true);

      if (timerReference.current !== null) {
        clearTimeout(timerReference.current);
      }

      timerReference.current = setTimeout(() => setCopied(false), 1500);
    } catch {
      setCopied(false);
    }
  }, [props.text]);

  return (
    <button
      className={styles.container}
      onClick={handleClick}
      data-copied={copied}
      aria-label={copied ? "コピーしました" : "コードをコピー"}
      type="button"
    >
      {copied ? (
        <svg
          width="16"
          height="16"
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          strokeWidth="2"
          strokeLinecap="round"
          strokeLinejoin="round"
          aria-hidden="true"
        >
          <polyline points="20 6 9 17 4 12" />
        </svg>
      ) : (
        <svg
          width="16"
          height="16"
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          strokeWidth="2"
          strokeLinecap="round"
          strokeLinejoin="round"
          aria-hidden="true"
        >
          <rect x="9" y="9" width="13" height="13" rx="2" ry="2" />
          <path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1" />
        </svg>
      )}
    </button>
  );
};
