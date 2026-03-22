import { Suspense } from "react";
import { MarkdownRenderer } from "@shared/components/global/mdx";
import { ArticleContentSkeleton } from "@shared/components/molecules/skeleton";
import { Series, SeriesSlug, ChapterSlug } from "@shared/domains/series";
import { Chapter, ChapterIdentifier } from "@shared/domains/series/chapter";
import { ChapterContainer } from "@shared/components/organisms/series/chapter";
import styles from "./index.module.css";

export type Props = {
  slug: SeriesSlug;
  chapterSlug: ChapterSlug;
  series: Series;
  renderer: MarkdownRenderer;
  findChapterBySlug: (slug: string) => Promise<Chapter>;
  findChaptersByIdentifiers: (identifiers: ChapterIdentifier[]) => Promise<Chapter[]>;
};

export const ChapterIndex = (props: Props) => (
  <div className={styles.container}>
    <div className={styles.wrapper}>
      <Suspense fallback={<ArticleContentSkeleton />}>
        <ChapterContainer
          slug={props.slug}
          chapterSlug={props.chapterSlug}
          series={props.series}
          renderer={props.renderer}
          findChapterBySlug={props.findChapterBySlug}
          findChaptersByIdentifiers={props.findChaptersByIdentifiers}
        />
      </Suspense>
    </div>
  </div>
);
