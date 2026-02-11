import { ReactNode } from "react";
import styles from "./field-row.module.css";

export type Props = {
  children: ReactNode;
  className?: string;
};

export const FormFieldRow = (props: Props) => {
  return (
    <div className={`${styles.container} ${props.className || ""}`}>
      {props.children}
    </div>
  );
};
