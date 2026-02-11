import { ReactNode } from "react";
import styles from "./form-label.module.css";

export type Props = {
  children: ReactNode;
  required?: boolean;
  htmlFor?: string;
  className?: string;
};

export const FormLabel = (props: Props) => {
  return (
    <label
      htmlFor={props.htmlFor}
      className={`${styles.container} ${props.className || ""}`}
    >
      {props.children}
      {props.required && <span className={styles.required}>*</span>}
    </label>
  );
};
