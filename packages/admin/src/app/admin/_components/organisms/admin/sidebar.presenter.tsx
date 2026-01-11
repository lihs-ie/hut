"use client";

import { type ReactNode } from "react";
import { usePathname } from "next/navigation";
import { AdminNavItem } from "@shared/components/molecules/navigation/admin-item";
import styles from "./sidebar.module.css";

export type NavItem = {
  href: string;
  icon: ReactNode;
  label: string;
};

export type Props = {
  items: NavItem[];
};

export const AdminSidebarPresenter = (props: Props) => {
  const currentPath = usePathname();

  return (
    <aside className={styles.container}>
      <nav className={styles.nav}>
        {props.items.map((item) => (
          <AdminNavItem
            key={item.href}
            href={item.href}
            icon={item.icon}
            label={item.label}
            isActive={currentPath.startsWith(item.href)}
          />
        ))}
      </nav>
    </aside>
  );
};
