import { forwardRef } from "react";
import styles from "./simple.module.css";

export type Props = {
  checked?: boolean;
  onChange?: (checked: boolean) => void;
  "aria-label"?: string;
};

export const SimpleSwitch = forwardRef<HTMLInputElement, Props>(
  (props: Props, ref) => {
    return (
      <label className={styles.container}>
        <input
          type="checkbox"
          ref={ref}
          checked={props.checked}
          onChange={(event) => props.onChange?.(event.target.checked)}
          className={styles.input}
          aria-label={props["aria-label"]}
        />
        <span className={styles.slider} />
      </label>
    );
  },
);

SimpleSwitch.displayName = "Switch";
