import { HTMLAttributes, ReactNode } from "react";
import styles from "./simple.module.css";

export type Props = {
  onClick?: () => void;
  disabled?: boolean;
  children: ReactNode;
  type?: HTMLButtonElement["type"];
};

export const SimpleButton = (props: Props) => (
  <button
    className={styles.container}
    onClick={props.onClick}
    disabled={props.disabled}
  >
    {props.children}
  </button>
);
