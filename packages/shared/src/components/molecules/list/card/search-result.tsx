import Link from "next/link";
import { MessageSquareIcon } from "@shared/components/atoms/icon/message";
import { BookOpenIcon } from "@shared/components/atoms/icon/facing-book";
import styles from "./search-result.module.css";
import { CommonImage } from "@shared/components/atoms/image/common";
import { ContentType } from "@shared/domains/search-token";
import { ContentTypeBadge } from "@shared/components/atoms/badge/content-type";
import { formatDate } from "@shared/aspects/date";

export type Props = {
  slug: string;
  type: ContentType;
  title: string;
  excerpt?: string;
  date: Date;
  tags: string[];
  imageUrl?: string;
  commentCount?: number;
  chapterCount?: number;
};

export const SearchResultCard = (props: Props) => {
  const getHref = () => {
    switch (props.type) {
      case ContentType.ARTICLE:
        return `/articles/${props.slug}`;
      case ContentType.MEMO:
        return `/memos/${props.slug}`;
      case ContentType.SERIES:
        return `/series/${props.slug}`;
      default:
        return "#";
    }
  };

  const getBadgeInfo = () => {
    switch (props.type) {
      case ContentType.ARTICLE:
        return { text: "記事", variant: "default" as const };
      case ContentType.MEMO:
        return { text: "スクラップ", variant: "secondary" as const };
      case ContentType.SERIES:
        return { text: "本", variant: "outline" as const };
      default:
        return { text: "記事", variant: "default" as const };
    }
  };

  const badgeInfo = getBadgeInfo();

  return (
    <Link href={getHref()} className={styles["card-link"]}>
      <div className={styles.container}>
        {props.imageUrl && (
          <div className={styles.image}>
            <CommonImage src={props.imageUrl} alt={props.title} />
          </div>
        )}
        <div className={styles["card-body"]}>
          <div className={styles.content}>
            <div className={styles.tags}>
              <ContentTypeBadge variant={badgeInfo.variant}>
                {badgeInfo.text}
              </ContentTypeBadge>
              {props.tags.map((tag) => (
                <span key={tag} className={styles.tag}>
                  {tag}
                </span>
              ))}
            </div>
            <h3 className={styles.title}>{props.title}</h3>
            {props.excerpt && <p className={styles.excerpt}>{props.excerpt}</p>}
          </div>
          <div className={styles.footer}>
            <div className={styles["footer-left"]}>
              <span>{formatDate(props.date)}</span>
            </div>
            <div className={styles["footer-right"]}>
              {props.type === ContentType.MEMO &&
                props.commentCount !== undefined && (
                  <div className={styles["icon-text"]}>
                    <MessageSquareIcon className={styles["small-icon"]} />
                    <span>{props.commentCount}</span>
                  </div>
                )}
              {props.type === ContentType.SERIES &&
                props.chapterCount !== undefined && (
                  <div className={styles["icon-text"]}>
                    <BookOpenIcon className={styles["small-icon"]} />
                    <span>{props.chapterCount}章</span>
                  </div>
                )}
            </div>
          </div>
        </div>
      </div>
    </Link>
  );
};
