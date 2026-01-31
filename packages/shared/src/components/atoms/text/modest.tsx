import { ReactNode } from "react";
import styles from "./modest.module.css";

export type Props = {
  children: ReactNode;
};

export const ModestText = (props: Props) => (
  <span className={styles.container}>{props.children}</span>
);
