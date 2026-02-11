import { MarkdownRenderer } from "@shared/components/global/mdx";
import { Memo } from "@shared/components/organisms/memo";
import { Memo as Aggregate } from "@shared/domains/memo";
import styles from "./index.module.css";
import { Tag } from "@shared/domains/attributes/tag";
import { Suspense } from "react";
import { Title } from "@shared/components/organisms/common/title";
import {
  ArticleTitleSkeleton,
  ArticleContentSkeleton,
} from "@shared/components/molecules/skeleton";
import { EngagementTracker } from "@shared/components/organisms/tracker";
import type {
  ContentType,
  SearchReferenceIdentifier,
} from "@shared/domains/search-token/reference";

export type Props = {
  findBySlug: (slug: string) => Promise<Aggregate>;
  slug: string;
  renderer: MarkdownRenderer;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
  incrementViewCount: (identifier: SearchReferenceIdentifier) => Promise<void>;
  tracking?: {
    contentType: ContentType;
    contentIdentifier: string;
  };
};

const MEMO_CONTENT_ELEMENT_ID = "memo-content";

export const MemoIndex = async (props: Props) => (
  <article className={styles.container}>
    <Suspense fallback={<ArticleTitleSkeleton />}>
      <Title
        find={props.findBySlug}
        slug={props.slug}
        titleOf={(memo) => memo.title}
        timelineOf={(memo) => memo.timeline}
      />
    </Suspense>
    <div id={MEMO_CONTENT_ELEMENT_ID} className={styles.content}>
      <Suspense fallback={<ArticleContentSkeleton />}>
        <Memo
          slug={props.slug}
          findBySlug={props.findBySlug}
          renderer={props.renderer}
          findAllTags={props.findAllTags}
          incrementViewCount={props.incrementViewCount}
        />
      </Suspense>
    </div>
    {props.tracking && (
      <EngagementTracker
        contentType={props.tracking.contentType}
        contentIdentifier={props.tracking.contentIdentifier}
        contentElementId={MEMO_CONTENT_ELEMENT_ID}
      />
    )}
  </article>
);
