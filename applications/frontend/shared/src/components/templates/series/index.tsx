import { SeriesDetail } from "@shared/components/organisms/series/detail";
import styles from "./index.module.css";
import { Series, SeriesSlug } from "@shared/domains/series";
import { Chapter, ChapterIdentifier } from "@shared/domains/series/chapter";
import { Tag } from "@shared/domains/attributes/tag";
import { Suspense } from "react";
import { ArticleContentSkeleton } from "@shared/components/molecules/skeleton";

export type Props = {
  slug: SeriesSlug;
  findBySlug: (slug: string) => Promise<Series>;
  findChaptersByIdentifiers: (identifiers: ChapterIdentifier[]) => Promise<Chapter[]>;
  findAllTags: (identifiers: string[]) => Promise<Tag[]>;
};

export const SeriesIndex = (props: Props) => (
  <div className={styles.container}>
    <Suspense fallback={<ArticleContentSkeleton />}>
      <SeriesDetail
        slug={props.slug}
        findBySlug={props.findBySlug}
        findChaptersByIdentifiers={props.findChaptersByIdentifiers}
        findAllTags={props.findAllTags}
      />
    </Suspense>
  </div>
);
