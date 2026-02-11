import { forwardRef, ReactNode } from "react";
import styles from "./button.module.css";

export type Props = {
  children: ReactNode;
  toggle: () => void;
  open?: boolean;
};

export const DropDownButton = forwardRef<HTMLDivElement, Props>(
  (props, ref) => (
    <div
      ref={ref}
      className={`${styles.container} ${props.open && styles.open}`}
      onClick={props.toggle}
    >
      {props.children}
    </div>
  )
);

DropDownButton.displayName = "DropDownButton";
