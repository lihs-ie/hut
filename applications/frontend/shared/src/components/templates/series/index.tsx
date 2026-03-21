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

export const SeriesIndex = async (props: Props) => {
  const series = await props.findBySlug(props.slug);
  const chapters = await props.findChaptersByIdentifiers(series.chapters);

  return (
    <div className={styles.container}>
      <Suspense fallback={<ArticleContentSkeleton />}>
        <SeriesDetail
          series={series}
          slug={props.slug}
          chapters={chapters}
          findAllTags={props.findAllTags}
        />
      </Suspense>
    </div>
  );
};
