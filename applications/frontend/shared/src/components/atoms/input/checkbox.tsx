import { forwardRef } from "react";
import styles from "./checkbox.module.css";

export type Props = {
  checked?: boolean;
  onChange?: (value: boolean) => void;
  className?: string;
  disabled?: boolean;
  label?: string;
  id?: string;
};

export const Checkbox = forwardRef<HTMLInputElement, Props>((props, ref) => {
  return (
    <label className={`${styles.container} ${props.className || ""}`}>
      <input
        ref={ref}
        type="checkbox"
        className={styles.input}
        checked={props.checked}
        onChange={(e) => props.onChange && props.onChange(e.target.checked)}
        disabled={props.disabled}
        id={props.id}
      />
      {props.label && <span className={styles.label}>{props.label}</span>}
    </label>
  );
});

Checkbox.displayName = "Checkbox";
