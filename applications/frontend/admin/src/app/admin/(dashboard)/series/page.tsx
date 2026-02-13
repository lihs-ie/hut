import { SeriesListIndex } from "@shared/components/templates/series/list";
import { search } from "@shared/actions/series";
import { Suspense } from "react";
import { ContentCardSkeletonList } from "@shared/components/molecules/skeleton";

const SeriesListLoader = async () => {
  const seriesList = await search({ slug: null, tags: null });

  return <SeriesListIndex seriesList={seriesList} />;
};

export default function AdminSeriesPage() {
  return (
    <Suspense fallback={<ContentCardSkeletonList count={6} />}>
      <SeriesListLoader />
    </Suspense>
  );
}
