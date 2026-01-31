import { ReactNode } from "react";
import styles from "./item.module.css";

export type Props = {
  children: ReactNode;
  onClick?: () => void;
};

export const DropDownItem = (props: Props) => (
  <div className={styles.container} onClick={props.onClick}>
    {props.children}
  </div>
);
