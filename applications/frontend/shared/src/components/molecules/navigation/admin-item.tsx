import { type ReactNode } from "react";
import { NavigableLink } from "@shared/components/molecules/link/navigable";
import styles from "./admin-item.module.css";

export type Props = {
  href: string;
  icon: ReactNode;
  label: string;
  isActive: boolean;
};

export const AdminNavItem = (props: Props) => (
  <NavigableLink
    href={props.href}
    className={`${styles.container} ${props.isActive ? styles.active : ""}`}
  >
    <span className={styles.icon}>{props.icon}</span>
    <span className={styles.label}>{props.label}</span>
  </NavigableLink>
);
