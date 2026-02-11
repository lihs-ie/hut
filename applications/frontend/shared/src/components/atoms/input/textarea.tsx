import { ClipboardEvent, forwardRef } from "react";
import styles from "./textarea.module.css";

export type Props = {
  placeholder?: string;
  value?: string;
  onChange?: (value: string) => void;
  onPaste?: (event: ClipboardEvent<HTMLTextAreaElement>) => void;
  className?: string;
  rows?: number;
  disabled?: boolean;
  min?: number;
  max?: number;
  name?: string;
};

export const Textarea = forwardRef<HTMLTextAreaElement, Props>((props, ref) => {
  return (
    <textarea
      ref={ref}
      className={`${styles.container} ${props.className || ""}`}
      placeholder={props.placeholder}
      value={props.value}
      onChange={(event) => props.onChange?.(event.target.value)}
      onPaste={props.onPaste}
      rows={props.rows || 4}
      disabled={props.disabled}
      minLength={props.min}
      maxLength={props.max}
      name={props.name}
      aria-multiline
    />
  );
});

Textarea.displayName = "Textarea";
