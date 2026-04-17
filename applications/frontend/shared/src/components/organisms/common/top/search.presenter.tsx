import Link from "next/link";

import styles from "./search.module.css";
import { HomeContentCard } from "@shared/components/molecules/list/card/home-content";
import { ContentEmpty } from "@shared/components/molecules/empty/content";
import { ContentType } from "@shared/domains/search-token";
import { Slug } from "@shared/domains/common";
import { TagName } from "@shared/domains/attributes/tag";
import { FileTextIcon } from "@shared/components/atoms/icon/file-text";
import { MessageIcon } from "@shared/components/atoms/icon/message";
import { BookOpenIcon } from "@shared/components/atoms/icon/facing-book";

export type LinkMode = "show" | "edit";

type ContentItem = {
  slug: Slug;
  type: ContentType;
  title: string;
  date: Date | null;
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

const emptyIconFor = (type: ContentType): React.ReactNode => {
  switch (type) {
    case ContentType.ARTICLE:
    case ContentType.CHAPTER:
      return <FileTextIcon className={styles.emptyicon} />;
    case ContentType.MEMO:
      return <MessageIcon className={styles.emptyicon} />;
    case ContentType.SERIES:
      return <BookOpenIcon className={styles.emptyicon} />;
    default:
      return undefined;
  }
};

export const ContentSectionPresenter = (props: Props) => {
  const displayContent = props.content
    .filter((item) => item.type === props.type)
    .slice(0, props.maxItems ?? 6);

  if (displayContent.length === 0) {
    return (
      <section className={styles.container}>
        <div className={styles.header}>
          <h2 className={styles.title}>{props.title}</h2>
        </div>
        <ContentEmpty
          title="まだコンテンツがありません"
          icon={emptyIconFor(props.type)}
        />
      </section>
    );
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
