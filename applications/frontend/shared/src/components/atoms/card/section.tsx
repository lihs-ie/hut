import { ReactNode } from "react";
import styles from "./section.module.css";

export type Props = {
  title: string;
  children: ReactNode;
};

export const SectionCard = (props: Props) => (
  <section className={styles.container}>
    <h2 className={styles.title}>{props.title}</h2>
    <div className={styles.content}>{props.children}</div>
  </section>
);
