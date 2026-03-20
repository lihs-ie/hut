"use client";

import { useState, useRef, useCallback, useEffect } from "react";
import styles from "./copy.module.css";
import { CheckIcon } from "@shared/components/atoms/icon/check";
import { ClipboardIcon } from "@shared/components/atoms/icon/clipboard";

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
        <CheckIcon className={styles.icon} />
      ) : (
        <ClipboardIcon className={styles.icon} />
      )}
    </button>
  );
};
