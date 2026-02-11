import { type ReactNode } from "react";
import Link from "next/link";
import styles from "./admin-item.module.css";

export type Props = {
  href: string;
  icon: ReactNode;
  label: string;
  isActive: boolean;
};

export const AdminNavItem = (props: Props) => (
  <Link
    href={props.href}
    className={`${styles.container} ${props.isActive ? styles.active : ""}`}
  >
    <span className={styles.icon}>{props.icon}</span>
    <span className={styles.label}>{props.label}</span>
  </Link>
);
