import { ReactNode } from "react";
import styles from "./simple.module.css";

export type Props = {
  children: ReactNode;
  value?: string;
  onChange: (value: string) => void;
};

export const SimpleSelect = (props: Props) => (
  <select
    value={props.value}
    onChange={(e) => props.onChange(e.target.value)}
    className={styles.container}
  >
    {props.children}
  </select>
);
