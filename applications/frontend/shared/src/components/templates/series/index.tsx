import { SeriesDetail } from "@shared/components/organisms/series/detail";
import styles from "./index.module.css";
import { Series } from "@shared/domains/series";
import { Suspense } from "react";
import { ArticleContentSkeleton } from "@shared/components/molecules/skeleton";

export type Props = {
  slug: string;
  findBySlug: (slug: string) => Promise<Series>;
};

export const SeriesIndex = (props: Props) => {
  return (
    <div className={styles.container}>
      <Suspense fallback={<ArticleContentSkeleton />}>
        <SeriesDetail
          series={props.findBySlug(props.slug)}
          slug={props.slug}
          author={props.author}
        />
      </Suspense>
    </div>
  );
};
