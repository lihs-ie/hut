import Link from "next/link";
import { ReactNode } from "react";
import styles from "./circle.module.css";

export type Props = {
  href: string;
  children: ReactNode;
};

export const CircleLink = (props: Props) => (
  <Link className={styles.container} href={props.href}>
    {props.children}
  </Link>
);
