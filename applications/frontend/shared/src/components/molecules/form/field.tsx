import { ReactNode } from "react";
import { FormLabel } from "@shared/components/atoms/text/form-label";
import styles from "./field.module.css";

export type Props = {
  label: string;
  required?: boolean;
  note?: string;
  children: ReactNode;
  className?: string;
  htmlFor?: string;
};

export const FormField = (props: Props) => {
  return (
    <div className={`${styles.container} ${props.className || ""}`}>
      <FormLabel htmlFor={props.htmlFor} required={props.required}>
        {props.label}
      </FormLabel>
      {props.children}
      {props.note && <p className={styles.note}>{props.note}</p>}
    </div>
  );
};
