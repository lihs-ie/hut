import { ChangeEvent, forwardRef } from "react";
import styles from "./text.module.css";

export type Props = {
  type?: "text" | "search" | "email" | "password";
  placeholder?: string;
  value?: string;
  onChange?: (value: string) => void;
  className?: string;
  autoFocus?: boolean;
  disabled?: boolean;
};

export const TextInput = forwardRef<HTMLInputElement, Props>((props, ref) => {
  return (
    <input
      ref={ref}
      type={props.type || "text"}
      className={`${styles.container} ${props.className || ""}`}
      placeholder={props.placeholder}
      value={props.value}
      onChange={(event: ChangeEvent<HTMLInputElement>) => {
        props.onChange && props.onChange(event.target.value);
      }}
      autoFocus={props.autoFocus}
      disabled={props.disabled}
    />
  );
});

TextInput.displayName = "TextInput";
