import Link from "next/link";
import styles from "./text.module.css";

export type Props = {
  href: string;
  children: React.ReactNode;
};

export const TextLink = (props: Props) => (
  <Link className={styles.container} href={props.href}>
    {props.children}
  </Link>
);
