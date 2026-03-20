import { SeriesListIndex } from "@shared/components/templates/series/list";
import { search } from "@shared/actions/series";
import { findAllTags } from "@shared/actions/tag";
import { Suspense } from "react";
import { ContentCardSkeletonList } from "@shared/components/molecules/skeleton";

export default function AdminSeriesPage() {
  return (
    <Suspense fallback={<ContentCardSkeletonList count={6} />}>
      <SeriesListIndex
        search={() => search({ slug: null, tags: null })}
        findAllTags={findAllTags}
      />
    </Suspense>
  );
}
