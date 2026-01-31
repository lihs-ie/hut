import { forwardRef } from "react";
import styles from "./month.module.css";

export type Props = {
  value?: string;
  onChange?: (value: Date) => void;
  className?: string;
  disabled?: boolean;
};

export const MonthInput = forwardRef<HTMLInputElement, Props>((props, ref) => {
  return (
    <input
      ref={ref}
      type="month"
      className={`${styles.container} ${props.className || ""}`}
      value={props.value}
      onChange={(event) =>
        props.onChange && props.onChange(new Date(event.target.value))
      }
      disabled={props.disabled}
    />
  );
});

MonthInput.displayName = "MonthInput";
