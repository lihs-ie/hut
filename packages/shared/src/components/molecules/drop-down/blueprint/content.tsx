import { forwardRef, ReactNode } from "react";
import styles from "./content.module.css";

export type Props = {
  children: ReactNode;
  open: boolean;
  top: number | null;
};

export const DropDownContent = forwardRef<HTMLDivElement, Props>(
  (props, ref) => (
    <div
      ref={ref}
      className={`${styles.container} ${props.open && styles.open}`}
      style={{ top: props.top ? `${props.top}px` : "100%" }}
    >
      {props.children}
    </div>
  )
);

DropDownContent.displayName = "DropDownContent";
