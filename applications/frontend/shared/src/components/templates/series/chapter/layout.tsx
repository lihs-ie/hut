import { ReactNode } from "react";
import styles from "./layout.module.css";

export type Props = {
  toc: ReactNode;
  children: ReactNode;
};

export const ChapterLayoutTemplate = (props: Props) => (
  <div className={styles.container}>
    {props.toc}
    {props.children}
  </div>
);
