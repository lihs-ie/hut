import Link from "next/link";
import styles from "./home-content.module.css";
import { ContentTypeBadge } from "@shared/components/atoms/badge/content-type";
import { TagName } from "@shared/domains/attributes/tag";
import { ContentType } from "@shared/domains/search-token";
import { Slug } from "@shared/domains/common";
import { Routes } from "@shared/config/presentation/route";
import { formatDate } from "@shared/aspects/date";

const defaultHref = (slug: Slug, type: ContentType) => {
  switch (type) {
    case ContentType.ARTICLE:
      return Routes.page.articles.show(slug);
    case ContentType.MEMO:
      return Routes.page.memos.show(slug);
    case ContentType.SERIES:
      return Routes.page.series.show(slug);
    default:
      return "#";
  }
};

const badgeText = (type: ContentType) => {
  switch (type) {
    case ContentType.ARTICLE:
      return { text: "記事", variant: "default" as const };
    case ContentType.MEMO:
      return { text: "メモ", variant: "secondary" as const };
    case ContentType.SERIES:
      return { text: "シリーズ", variant: "outline" as const };
    default:
      return { text: "", variant: "default" as const };
  }
};

type Props = {
  slug: Slug;
  type: ContentType;
  title: string;
  excerpt?: string;
  date: Date;
  tagNames: TagName[];
  href?: string;
};

export const HomeContentCard = (props: Props) => {
  const badge = badgeText(props.type)!;

  return (
    <Link
      href={props.href ?? defaultHref(props.slug, props.type)}
      className={styles.container}
      scroll={true}
    >
      <article className={styles.card}>
        <div className={styles.header}>
          <div className={styles.meta}>
            <ContentTypeBadge variant={badge.variant} className={styles.badge}>
              {badge.text}
            </ContentTypeBadge>
            <span className={styles.date}>
              {formatDate(props.date)}
            </span>
          </div>
        </div>

        <div className={styles.body}>
          <h2 className={styles.title}>{props.title}</h2>
          {props.tagNames.length > 0 && (
            <div className={styles.tags}>
              {props.tagNames.slice(0, 3).map((tag) => (
                <span key={tag} className={styles.tag}>
                  #{tag}
                </span>
              ))}
            </div>
          )}
        </div>
      </article>
    </Link>
  );
};
