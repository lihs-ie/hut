import Link from "next/link";
import { Routes } from "@shared/config/presentation/route";
import { DropDownItem } from "./blueprint/item";
import { DropDown } from "./blueprint";
import { ReactNode } from "react";
import { FileTextIcon } from "@shared/components/atoms/icon/file-text";
import { BallpenIcon } from "@shared/components/atoms/icon/ballpen";
import styles from "./post-menu.module.css";

const items: Record<string, ReactNode> = {
  article: (
    <Link href={Routes.page.articles.new} className={styles.content}>
      <i className={styles.icon}>
        <FileTextIcon />
      </i>
      <p className={styles.text}>記事</p>
    </Link>
  ),
};

export const PostMenuDropDown = () => (
  <DropDown
    content={Object.values(items).map((item, index) => (
      <DropDownItem key={index}>{item}</DropDownItem>
    ))}
  >
    <span className={styles.trigger}>
      <BallpenIcon className={styles.triggerIcon} />
    </span>
  </DropDown>
);
