"use client";

import { usePathname } from "next/navigation";
import { NavigableLink } from "@shared/components/molecules/link/navigable";
import styles from "./chapter-toc-link.module.css";

export type Props = {
  href: string;
  className: string;
  activeClassName: string;
  children: React.ReactNode;
};

export const ChapterTocLink = (props: Props) => {
  const pathname = usePathname();
  const isActive = pathname === props.href;
  const composedClassName = `${styles.container} ${isActive ? props.activeClassName : props.className}`;

  return (
    <NavigableLink
      href={props.href}
      className={composedClassName}
      aria-current={isActive ? "page" : undefined}
    >
      {props.children}
    </NavigableLink>
  );
};
