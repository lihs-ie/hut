import Link from "next/link";

import styles from "./search.module.css";
import { HomeContentCard } from "@shared/components/molecules/list/card/home-content";
import { ContentType } from "@shared/domains/search-token";
import { Slug } from "@shared/domains/common";
import { TagName } from "@shared/domains/attributes/tag";

export type LinkMode = "show" | "edit";

type ContentItem = {
  slug: Slug;
  type: ContentType;
  title: string;
  date: Date;
  tagNames: TagName[];
  excerpt?: string;
  href?: string;
};

export type Props = {
  title: string;
  type: ContentType;
  content: ContentItem[];
  viewAllLink?: string;
  maxItems?: number;
};

export const ContentSectionPresenter = (props: Props) => {
  const displayContent = props.content
    .filter((item) => item.type === props.type)
    .slice(0, props.maxItems ?? 6);

  if (displayContent.length === 0) {
    return null;
  }

  return (
    <section className={styles.container}>
      <div className={styles.header}>
        <h2 className={styles.title}>{props.title}</h2>
        {props.viewAllLink && (
          <Link href={props.viewAllLink} className={styles.viewAll}>
            もっと見る →
          </Link>
        )}
      </div>

      <div className={styles.list}>
        {displayContent.map((item) => (
          <HomeContentCard
            key={item.slug}
            {...item}
            tagNames={item.tagNames}
            href={item.href}
          />
        ))}
      </div>
    </section>
  );
};
