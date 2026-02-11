import { Article } from "@shared/domains/articles";
import { Suspense } from "react";
import { Article as ArticleComponent } from "@shared/components/organisms/article";
import { MarkdownRenderer, Node } from "@shared/components/global/mdx";
import styles from "./index.module.css";
import {
  ArticleTitleSkeleton,
  ArticleContentSkeleton,
  ArticleSidebarSkeleton,
} from "@shared/components/molecules/skeleton";
import { Sidebar } from "@shared/components/organisms/article/sidebar";
import { Title } from "@shared/components/organisms/common/title";
import { Tag } from "@shared/domains/attributes/tag";
import { EngagementTracker } from "@shared/components/organisms/tracker";
import type {
  ContentType,
  SearchReferenceIdentifier,
} from "@shared/domains/search-token/reference";

export type Props = {
  slug: string;
  article: {
    renderer: MarkdownRenderer;
    findBySlug: (slug: string) => Promise<Article>;
    findAllTags: (identifiers: string[]) => Promise<Tag[]>;
    incrementViewCount: (identifier: SearchReferenceIdentifier) => Promise<void>;
  };
  sidebar: {
    createTableOfContents: (slug: string) => Promise<Node[]>;
  };
  tracking?: {
    contentType: ContentType;
    contentIdentifier: string;
  };
};

const ARTICLE_CONTENT_ELEMENT_ID = "article-content";

export const ArticleIndex = async (props: Props) => (
  <div className={styles.container}>
    <Suspense fallback={<ArticleTitleSkeleton />}>
      <Title
        find={props.article.findBySlug}
        slug={props.slug}
        titleOf={(content) => content.title}
        timelineOf={(content) => content.timeline}
      />
    </Suspense>
    <div id={ARTICLE_CONTENT_ELEMENT_ID} className={styles.content}>
      <Suspense fallback={<ArticleContentSkeleton />}>
        <ArticleComponent
          slug={props.slug}
          renderer={props.article.renderer}
          findBySlug={props.article.findBySlug}
          findAllTags={props.article.findAllTags}
          incrementViewCount={props.article.incrementViewCount}
        />
      </Suspense>
      <div className={styles.sidebar}>
        <Suspense fallback={<ArticleSidebarSkeleton />}>
          <Sidebar
            createTableOfContents={props.sidebar.createTableOfContents}
            slug={props.slug}
          />
        </Suspense>
      </div>
    </div>
    {props.tracking && (
      <EngagementTracker
        contentType={props.tracking.contentType}
        contentIdentifier={props.tracking.contentIdentifier}
        contentElementId={ARTICLE_CONTENT_ELEMENT_ID}
      />
    )}
  </div>
);
