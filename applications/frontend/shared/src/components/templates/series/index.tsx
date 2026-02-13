import { SeriesDetail } from "@shared/components/organisms/series/detail";
import styles from "./index.module.css";
import { Series } from "@shared/domains/series";
import { slugSchema } from "@shared/domains/common/slug";
import { Suspense } from "react";
import { ArticleContentSkeleton } from "@shared/components/molecules/skeleton";

export type Props = {
  slug: string;
  findBySlug: (slug: string) => Promise<Series>;
};

const SeriesDetailLoader = async (props: Props) => {
  const series = await props.findBySlug(props.slug);
  const seriesSlug = slugSchema.parse(props.slug);

  return <SeriesDetail series={series} slug={seriesSlug} />;
};

export const SeriesIndex = (props: Props) => {
  return (
    <div className={styles.container}>
      <Suspense fallback={<ArticleContentSkeleton />}>
        <SeriesDetailLoader
          slug={props.slug}
          findBySlug={props.findBySlug}
        />
      </Suspense>
    </div>
  );
};
