import { SeriesDetail } from "@shared/components/organisms/series/detail";
import styles from "./index.module.css";
import { Series, SeriesSlug } from "@shared/domains/series";
import { Suspense } from "react";
import { ArticleContentSkeleton } from "@shared/components/molecules/skeleton";

export type Props = {
  slug: SeriesSlug;
  findBySlug: (slug: string) => Promise<Series>;
  author?: {
    name: string;
    avatar?: string;
    bio?: string;
  };
};

export const SeriesIndex = async (props: Props) => {
  const series = await props.findBySlug(props.slug);

  return (
    <div className={styles.container}>
      <Suspense fallback={<ArticleContentSkeleton />}>
        <SeriesDetail
          series={series}
          slug={props.slug}
          author={props.author}
        />
      </Suspense>
    </div>
  );
};
