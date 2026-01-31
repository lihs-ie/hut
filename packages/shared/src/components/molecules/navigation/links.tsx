import { Routes } from "@shared/config/presentation/route";
import Link from "next/link";
import styles from "./links.module.css";
import { ReactNode } from "react";

const Href = {
  ARTICLES: Routes.page.articles.index,
  // [初期リリース対象外] SERIES: Routes.page.series.index,
  MEMOS: Routes.page.memos.index,
  ABOUT: Routes.page.about,
} as const;

type Href = (typeof Href)[keyof typeof Href];

const links: { href: Href; label: ReactNode }[] = [
  { href: Routes.page.articles.index, label: "Articles" },
  // [初期リリース対象外] { href: Routes.page.series.index, label: "Series" },
  { href: Routes.page.memos.index, label: "Memos" },
  { href: Routes.page.about, label: "About" },
];

export type Props = {
  current?: Href;
};

export const NavigationLinks = (props: Props) => (
  <nav className={styles.container}>
    {links.map((link, index) => (
      <Link
        key={index}
        href={link.href}
        className={`${styles.link} ${
          props.current === link.href && styles.current
        }`}
      >
        {link.label}
      </Link>
    ))}
  </nav>
);
