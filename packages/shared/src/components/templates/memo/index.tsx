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

export type Props = {
  findBySlug: (slug: string) => Promise<Aggregate>;
  slug: string;
  renderer: MarkdownRenderer;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
};

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
    <div className={styles.content}>
      <Suspense fallback={<ArticleContentSkeleton />}>
        <Memo
          slug={props.slug}
          findBySlug={props.findBySlug}
          renderer={props.renderer}
          findAllTags={props.findAllTags}
        />
      </Suspense>
    </div>
  </article>
);
