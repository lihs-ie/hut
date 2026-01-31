import { ReactNode } from "react";
import styles from "./section-header.module.css";

export type Props = {
  title: string;
  children?: ReactNode;
  className?: string;
};

export const SectionHeader = (props: Props) => {
  return (
    <div className={`${styles.container} ${props.className || ""}`}>
      <h2 className={styles.title}>{props.title}</h2>
      {props.children}
    </div>
  );
};
