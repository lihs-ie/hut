import Link from "next/link";
import { Routes } from "@shared/config/presentation/route";
import { DropDownItem } from "./blueprint/item";
import { DropDown } from "./blueprint";
import { ReactNode } from "react";
import { FileTextIcon } from "@shared/components/atoms/icon/file-text";
// [初期リリース対象外] import { FacingBookIcon } from "@shared/components/atoms/icon/facing-book";
import { MessageIcon } from "@shared/components/atoms/icon/message";
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
  // [初期リリース対象外]
  // series: (
  //   <Link href={Routes.page.series.new} className={styles.content}>
  //     <i className={styles.icon}>
  //       <FacingBookIcon />
  //     </i>
  //     <p className={styles.text}>連載</p>
  //   </Link>
  // ),
  memo: (
    <Link href={Routes.page.memos.new} className={styles.content}>
      <i className={styles.icon}>
        <MessageIcon />
      </i>
      <p className={styles.text}>メモ</p>
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
      <span className={styles.triggerText}>投稿する</span>
    </span>
  </DropDown>
);
