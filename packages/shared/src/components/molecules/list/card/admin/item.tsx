import { type ReactNode } from "react";
import Link from "next/link";
import styles from "./item.module.css";

export type Badge = {
  type: "closed" | "open" | "visibility";
  label: string;
  icon?: ReactNode;
};

export type MetaItem = {
  label: string;
  icon?: ReactNode;
};

export type Props = {
  href: string;
  title: string;
  badges?: Badge[];
  meta?: MetaItem[];
};

export const AdminItemCard = (props: Props) => (
  <Link href={props.href} className={styles.container}>
    <div className={styles.header}>
      <h3 className={styles.title}>{props.title}</h3>
      {props.badges && props.badges.length > 0 && (
        <div className={styles.badges}>
          {props.badges.map((badge, index) => (
            <span
              key={index}
              className={`${styles.badge} ${styles[`badge-${badge.type}`]}`}
            >
              {badge.icon && (
                <span className={styles["meta-icon"]}>{badge.icon}</span>
              )}
              {badge.label}
            </span>
          ))}
        </div>
      )}
    </div>
    {props.meta && props.meta.length > 0 && (
      <div className={styles.meta}>
        {props.meta.map((item, index) => (
          <span key={index} className={styles["meta-item"]}>
            {item.icon && (
              <span className={styles["meta-icon"]}>{item.icon}</span>
            )}
            {item.label}
          </span>
        ))}
      </div>
    )}
  </Link>
);
